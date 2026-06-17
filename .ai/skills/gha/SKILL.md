---
name: hardening-github-actions
description: Writes and reviews GitHub Actions workflows with security hardening. Use when creating, modifying, or reviewing .github/workflows/*.yml or .github/actions/*/action.yml files. Covers shell injection prevention, trust gating for fork PRs, action pinning, and secrets hygiene.
---

# Hardening GitHub Actions

This guidance assumes a **public repository** threat model where untrusted users can open fork PRs. The fork trust-gating sections are primarily relevant in that context. Shell injection prevention, action pinning, credential hygiene, and checkout persistence apply to all repos.

## Shell injection prevention

**Never interpolate `${{ }}` expressions directly into `run:` blocks.** GitHub expression substitution happens *before* the shell parses the command. Attacker-controlled values (branch names, PR titles, input fields) can inject arbitrary shell commands.

Unsafe values include anything derived from:
- `github.event.pull_request.head.ref` (fork branch names allow shell metacharacters)
- `github.event.pull_request.title` / `.body`
- `github.event.inputs.*` (free-form text)
- `github.event.comment.body`
- `github.event.pull_request.head.repo.full_name` (fork repo names)

### Fix: use intermediate environment variables

Pass untrusted expressions via `env:` on the step, then reference them as quoted shell variables. Environment variables are set at runtime and are not subject to shell expansion.

```yaml
# BAD - shell injection via branch name
- run: git fetch origin ${{ github.event.pull_request.head.ref }}

# GOOD - safe via env
- env:
    HEAD_REF: ${{ github.event.pull_request.head.ref }}
  run: git fetch origin "$HEAD_REF"
```

This applies to **all** `${{ }}` references in `run:` blocks that touch event data or action inputs. Expressions used only in `with:`, `if:`, or `env:` values (not shell) are fine.

Ref: https://docs.github.com/actions/security-guides/security-hardening-for-github-actions#using-an-intermediate-environment-variable

### Safe contexts (no fix needed)

- `github.actor` (alphanumeric + hyphens only)
- `github.repository` (org/repo, restricted charset)
- `github.sha`, `github.run_id`, `github.run_number` (hex/numeric)
- Values used only in `if:`, `with:`, `run-name:`, `concurrency.group:`, or other non-shell YAML fields

## Trust gating for fork PRs

Fork PRs on `pull_request` triggers get a read-only `GITHUB_TOKEN` and empty `secrets.*` (platform-enforced). But composite actions, checkout steps, and shell commands still execute. Structure workflows to avoid running untrusted code with privileges.

### Preferred: two-job structure

```yaml
jobs:
  detect:
    runs-on: ubuntu-latest
    outputs:
      is_trusted: ${{ steps.check.outputs.is_trusted }}
    steps:
      - uses: actions/checkout@<pinned-sha>
        with:
          ref: ${{ github.event.pull_request.base.sha || github.sha }}
      - id: check
        run: # ... determine trust level

  build:
    needs: detect
    if: needs.detect.outputs.is_trusted == 'true'
    runs-on: ubuntu-latest
    steps:
      # ... privileged steps with secrets
```

Key properties:
- **Job 1 checks out trusted code only** (`base.sha` or `github.sha`, never the fork's HEAD)
- **Job 2 never spawns for untrusted forks** -- composite actions, checkout-merge, and all secret-bearing steps are unreachable
- Step-level `if:` guards are fragile (earlier steps still execute); job-level `if:` is definitive

### Avoid: step-level early exit

A step like `if: is_trusted == 'false'` then `exit` leaves all preceding steps exposed to injection. Use this only as a temporary measure.

### Why `pull_request` not `pull_request_target`

Use `pull_request` triggers, not `pull_request_target`. With `pull_request`, GitHub runs the workflow from the **base branch** -- fork PRs cannot modify the workflow or composite actions that execute. With `pull_request_target`, the workflow runs with write token and secrets but can be tricked into checking out and executing fork code. If `pull_request_target` is ever needed, it must never check out PR code.

### Detect-job must check out base code only

The detect/gate job must use `ref: ${{ github.event.pull_request.base.sha || github.sha }}` to ensure it only runs trusted composite actions from the base branch. If this were changed to check out the PR head, a fork could replace the detect action itself.

### `workflow_dispatch` trust assumptions

Manual dispatch with a PR number is treated as trusted because only maintainers/admins can trigger `workflow_dispatch`. Ensure repository settings continue to restrict dispatch permissions -- if collaborators with lower access levels gain `actions: write`, they could dispatch against a fork PR and run its code with secrets.

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

Always set `persist-credentials: false` on `actions/checkout`. The default (`true`) writes the `GITHUB_TOKEN` into `.git/config`, making it readable by every subsequent step -- including third-party actions and build tools. With `contents: write` permission, a compromised step could push to the repository.

```yaml
- uses: actions/checkout@<pinned-sha>
  with:
    persist-credentials: false
```

If git push is needed later, configure credentials explicitly for just that step.

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

When writing to `$GITHUB_OUTPUT` with `echo "key=$value"`, an attacker who controls `value` can inject newlines to set arbitrary output keys. Use the multiline delimiter format when the value could contain newlines (e.g., PR titles, commit messages, comment bodies):

```bash
{
  echo "title<<EOF"
  echo "$PR_TITLE"
  echo "EOF"
} >> "$GITHUB_OUTPUT"
```

Values with restricted charsets (git branch names, numeric IDs, booleans computed from restricted inputs) are safe with the simple `echo "key=$value"` format.

## `workflow_run` artifact poisoning

If adding `workflow_run` triggers: a fork PR can upload artifacts via `pull_request`, and a `workflow_run` job (which has secrets) may then download and process them. Never execute or trust artifact contents from untrusted runs without validation.

## Review checklist

When reviewing or writing a workflow:

1. Grep for `${{ ` inside `run:` blocks -- each one is a potential injection point
2. For each match, determine if the value is attacker-controlled; if so, move to `env:`
3. Verify fork PRs cannot reach secret-bearing steps (prefer two-job gate)
4. Confirm third-party actions are SHA-pinned
5. Validate any free-form `workflow_dispatch` inputs
6. Check that `pull_request_target` is not used (or if it is, that it never checks out PR code)
7. Verify no `accept-flake-config = true` or `--accept-flake-config` in Nix steps (prevents flake nixConfig escape)
8. Verify `persist-credentials: false` on all `actions/checkout` steps
9. Verify `workflow_dispatch` permissions remain restricted to maintainers/admins
