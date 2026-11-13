---
name: hardening-github-actions
description: Writes and reviews GitHub Actions workflows with security hardening. Use when creating, modifying, or reviewing .github/workflows/*.yml or .github/actions/*/action.yml files. Covers shell injection prevention, unprivileged fork CI, trusted publishing workflows, action pinning, and secrets hygiene.
---

# Hardening GitHub Actions

This guidance assumes a **public repository** threat model where untrusted users can open fork PRs. The fork CI sections are primarily relevant in that context. Shell injection prevention, action pinning, credential hygiene, and checkout persistence apply to all repos.

## Shell injection prevention

**Never interpolate `${{ }}` expressions directly into `run:` blocks, even for trusted values.** Pass values through `env:` and use quoted shell variables instead. GitHub expression substitution happens *before* the shell parses the command. This uniform rule avoids trust-classification mistakes and remains safe if a value's source changes later.

Unsafe values include anything derived from:
- `github.event.pull_request.head.ref` (fork branch names allow shell metacharacters)
- `github.event.pull_request.title` / `.body`
- `github.event.inputs.*` (free-form text)
- `github.event.comment.body`
- `github.event.pull_request.head.repo.full_name` (fork repo names)

### Fix: use intermediate environment variables

Pass expressions via `env:` on the step, then reference them as quoted shell variables. Their contents are not reparsed as shell syntax. Validate values for the receiving command too: quoting alone does not prevent option injection, unsafe paths, or execution through `eval`.

```yaml
# BAD - shell injection via branch name
- run: git fetch origin ${{ github.event.pull_request.head.ref }}

# Prevents shell injection; validate the ref for the intended operation too.
- env:
    HEAD_REF: ${{ github.event.pull_request.head.ref }}
  run: git fetch origin "$HEAD_REF"
```

This applies to **all** `${{ }}` references in `run:` blocks, including SHAs, numeric IDs, constants, and trusted inputs. Other YAML fields do not automatically make input safe: an action may execute a `with:` input (for example, `actions/github-script`'s `script`), and tools may interpret environment variables as executable configuration.

This is a placement rule, not a requirement to add format validation for every context value. GitHub-generated `github.sha`, `github.run_id`, and `github.run_number` need no extra hex/numeric checks solely for shell-injection prevention. Likewise, do not add username/repository-name regex checks to `github.actor` or `github.repository` merely to pass them through `env:`. Validate attacker-supplied inputs and operation-specific requirements where needed; authorization checks remain separate.

Ref: https://docs.github.com/actions/security-guides/security-hardening-for-github-actions#using-an-intermediate-environment-variable

### Distinguish shell injection from authorization

Expressions remain appropriate in data and condition fields such as `env:`, `if:`, `run-name:`, and `concurrency.group:`. Safe expression placement does not establish authorization: actor identity and the executing repository's name do not prove where the PR's code came from.

## Secure fork CI and privileged workflows

Fork PRs on `pull_request` triggers normally get a read-only `GITHUB_TOKEN` and no repository secrets (the token itself remains available). These platform restrictions are the security boundary; verify repository settings do not grant fork runs write tokens or secrets. The workflow runs in the PR merge context, so a fork can change workflow definitions and checked-out composite actions as well as build code.

### Recommended: unprivileged PR CI, separate trusted publishing

Run fork builds and tests with `pull_request`, read-only permissions, no repository secrets, and GitHub-hosted runners. Do not give these jobs access to privileged self-hosted runners or internal services. The following is a standalone PR workflow; adapt the test command to the project:

```yaml
name: PR checks
on:
  pull_request:
permissions:
  contents: read
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@d23441a48e516b6c34aea4fa41551a30e30af803 # v6.1.0
        with:
          persist-credentials: false
      # Install any required tools with SHA-pinned actions.
      - run: ./ci/test.sh
```

Put publishing or deployment in a **separate workflow** triggered by a push to a protected branch after review. Require review of workflow changes through branch protection/rulesets, and restrict bypass permissions. Build from that push's commit rather than a PR ref or artifacts from a fork run. For example, adapt `main` and the illustrative build/publish scripts below:

```yaml
name: Publish reviewed code
on:
  push:
    branches: [main]
permissions:
  contents: read
cache-mode: none
jobs:
  publish:
    runs-on: ubuntu-latest
    permissions:
      contents: write # Example: publishing a GitHub release.
    steps:
      - uses: actions/checkout@d23441a48e516b6c34aea4fa41551a30e30af803 # v6.1.0
        with:
          ref: ${{ github.sha }}
          persist-credentials: false
      # Install any required tools with SHA-pinned actions.
      - run: ./ci/build-release.sh
      - run: ./ci/publish-release.sh
        env:
          GH_TOKEN: ${{ github.token }}
```

Grant only the permissions the publishing operation needs. If deployment credentials are needed, store them in a GitHub environment restricted to the protected branch, with required reviewers where appropriate. These controls must be configured in repository settings; writing an environment name in YAML does not create those restrictions.

The boundary is GitHub's restrictions on fork runs plus protected access to the publishing branch and credentials. A fork can rewrite its PR workflow, but cannot use that to obtain the protected publishing credentials. A passing PR check or maintainer approval to run fork CI does not by itself make the fork's code trusted for privileged execution.

### Job gates are supplementary controls

In a trusted workflow, a detection job followed by a job-level `if:` can keep unwanted events away from every step of a privileged job. Prefer that over an early-exit step, which leaves preceding steps exposed. A gate in a fork-editable `pull_request` workflow is not an independent security boundary: the fork can change or remove it.

If a gate needs a local composite action, load it from the trusted workflow revision before any PR checkout. On `pull_request`, checking out `base.sha` protects only the checked-out detector; it cannot protect the fork-editable workflow definition. On `pull_request_target`, `github.sha` identifies the default-branch workflow commit; on `workflow_dispatch`, it identifies the selected ref's commit, which must be reviewed and trusted.

A gate must authorize the operation and exact source revision. For a same-repository policy, compare `github.event.pull_request.head.repo.full_name` with `github.repository`; checking only `github.repository` or its owner does not exclude forks. Verify current repository permissions for human authorizers. A bot actor, prior contribution, label, or maintainer rerun alone does not authorize the fork's current code. Bind any manual approval to an immutable head SHA. Keep fork tests isolated from privileged jobs even after approval.

### Privileged PR metadata operations

`pull_request_target` runs the base repository's **default-branch** workflow and can access write permissions and secrets. Ordinary fork-run approval settings do not gate this event. Reserve it for operations such as labeling or commenting; prefer API metadata over checking out fork content. Treat titles, paths, filenames, symlinks, configuration, and artifacts as untrusted too: a trusted tool can expose credentials or execute code while processing them. Do not execute PR code, directly or indirectly, in a privileged job.

GitHub.com changed `pull_request_target` to always use the default branch on December 8, 2025. Older articles describing workflow selection from arbitrary PR target branches are historical; this change did **not** make `pull_request` workflows trusted. Check current documentation and the deployed GitHub Enterprise Server version when relying on platform protections.

Refs: [current security guidance](https://docs.github.com/en/actions/reference/security/securely-using-pull_request_target), [workflow-source change](https://github.blog/changelog/2025-11-07-actions-pull_request_target-and-environment-branch-protections-changes/).

### `workflow_dispatch` trust assumptions

A maintainer dispatching a workflow does not make a supplied PR number, ref, or artifact trusted. Keep privileged manual workflows on protected, reviewed refs and validate inputs before use. Do not dispatch a secret-bearing build against fork code. Restrict dispatch access and enforce environment branch restrictions and approvals in repository settings when using deployment credentials.

For unprivileged manual PR tests, require the full reviewed head SHA and verify that it matches the PR before fetching. Resolve the base SHA once, then merge those exact commits; do not fetch moving branch tips after verification. Keep the run read-only and secret-free, and set `cache-mode: none` so fork code cannot populate the dispatch branch's cache scope. Prefer approving the ordinary `pull_request` run when manual checkout is unnecessary.

`permissions:` limits `GITHUB_TOKEN`, not cache access. Current GitHub.com defaults restrict default-branch cache writes for certain untrusted triggers, but `workflow_dispatch` retains write-capable caching by default. Do not infer cache isolation from a read-only repository token or from the absence of an explicit cache-save step.

Ref: https://docs.github.com/en/actions/reference/workflows-and-actions/dependency-caching#cache-access-for-low-trust-workflow-triggers

## Action pinning

Pin third-party actions to full commit SHAs, not mutable tags. Tags like `v4` can be force-pushed.

```yaml
# BAD
- uses: actions/checkout@v4

# GOOD
- uses: actions/checkout@34e114876b0b11c390a56381ad16ebd13914f8d5 # v4
```

Look up SHAs with: `git ls-remote --tags https://github.com/<owner>/<repo>.git <tag>`

Keep the tag as a trailing comment for readability.

## Input validation

Validate free-form `workflow_dispatch` inputs before use:

```yaml
- env:
    PR_NUMBER: ${{ github.event.inputs.pr_number }}
  run: |
    if ! [[ "$PR_NUMBER" =~ ^[0-9]+$ ]]; then
      echo "::error::pr_number must be numeric"
      exit 1
    fi
```

## Checkout credential persistence

Always set `persist-credentials: false` on `actions/checkout` in this repository. The default (`true`) persists credentials on disk: older versions use `.git/config`; v6 and later use `$RUNNER_TEMP`. Either location can expose credentials to subsequent code in the job. Disabling persistence reduces exposure but does not isolate untrusted code from the job's token or secrets.

```yaml
- uses: actions/checkout@<pinned-sha>
  with:
    persist-credentials: false
```

If a trusted job must push, configure credentials explicitly for that step. Step placement reduces exposure but does not isolate credentials from other code in the job. A job gate inside a fork-editable workflow cannot establish trust.

## Secrets hygiene in `run:` blocks

Pass secrets through `env:` rather than inline `${{ secrets.* }}` in shell:

```yaml
# BAD - secret in shell substitution, visible in logs on syntax error
- run: echo "${{ secrets.KEY }}" > keyfile

# GOOD
- env:
    KEY: ${{ secrets.KEY }}
  run: echo "$KEY" > keyfile
```

## `GITHUB_OUTPUT` injection

When writing to `$GITHUB_OUTPUT`, an attacker-controlled value containing newlines can inject additional output keys. A fixed multiline delimiter such as `EOF` is also unsafe: the value can contain that delimiter on its own line. For arbitrary text, use a file or serialize to a single-line JSON string, for example with `jq`:

```bash
title_json=$(jq -cn --arg value "$PR_TITLE" '$value')
printf 'title_json=%s\n' "$title_json" >> "$GITHUB_OUTPUT"
```

Decode with `fromJSON` into a step's `env:`, then use a quoted shell variable; never insert decoded text into executable script source. If using a multiline output, generate a fresh delimiter and verify it cannot occur as a delimiter line in the value. Apply the same framing rules to `$GITHUB_ENV`, and never let untrusted data choose environment-variable names.

Validated numeric IDs, SHAs, and booleans can use the simple `printf 'key=%s\n' "$value"` format. Safe output framing does not make a value trusted by its consumer.

## `workflow_run` artifact poisoning

If privileged reporting requires a `workflow_run` follow-up, keep all PR execution in the unprivileged producer. Select artifacts using the triggering run's ID and verify the expected repository, workflow, event, and head SHA; a successful run does not make its artifacts trusted. Parse only the required data with strict validation. Extract away from trusted code and do not let artifacts overwrite scripts, supply tool configuration or environment-variable names, or enter executable expressions. Rebuild releases from reviewed commits rather than publishing fork-produced binaries.

Refs: [Apache's workflow and artifact guidance](https://cwiki.apache.org/confluence/spaces/BUILDS/pages/321719166/GitHub%2BActions%2BSecurity), [zizmor security rules](https://docs.zizmor.sh/audits/). Cross-check historical platform claims against current GitHub documentation.

## Review checklist

When reviewing or writing a workflow:

1. Search for `${{` inside `run:` blocks, including expressions without a following space
2. Move every match to `env:` and use quoted shell variables, regardless of whether the value is trusted
3. Keep fork CI unprivileged; place publishing in a separate trusted workflow and verify the repository settings that protect its branch and credentials
4. Confirm third-party actions are SHA-pinned
5. Validate any free-form `workflow_dispatch` inputs
6. For `pull_request_target` or `workflow_run`, trace all untrusted metadata, files, and artifacts into privileged operations; checking for PR script execution alone is insufficient
7. Verify no `accept-flake-config = true` or `--accept-flake-config` in Nix steps (prevents flake nixConfig escape)
8. Verify `persist-credentials: false` on every checkout; configure any required push credentials explicitly in a trusted job
9. Verify privileged `workflow_dispatch` runs use reviewed refs and cannot execute fork code through inputs or artifacts
10. Check cache access and runner isolation independently of token permissions; use `cache-mode: none` for manual fork tests and release jobs
