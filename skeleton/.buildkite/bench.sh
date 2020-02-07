#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix coreutils gnugrep gawk time haskellPackages.hp2pretty buildkite-agent

set -euo pipefail

bench_name=iohk-skeleton-bench
total_time=total-time.txt

echo "--- Build"
# TODO: remove sourcesOverride
nix-build -A benchmarks.iohk-skeleton.iohk-skeleton-bench
bench=./result/bin/iohk-skeleton-bench

echo "+++ Run benchmark"

command time -o $total_time -v $bench --json $bench_name.json -o $bench_name.html +RTS -N2 -qg -A1m -I0 -T -M8G -h -RTS 2>&1 | tee $bench_name.log

printf 'Link to \033]1339;url=artifact://'$bench_name.html';content='"Benchmark Report"'\a\n'

hp2pretty $bench_name.hp

if [ -n "${BUILDKITE:-}" ]; then
  echo "--- Upload"
  buildkite-agent artifact upload "$bench_name.html"
  buildkite-agent artifact upload "$bench_name.json"

  # Requires buildkite-agent 3.x
  # cat << EOF | buildkite-agent annotate --style "info"
  # Read the <a href="artifact://$bench_name.html">benchmark results</a>
  # EOF

  buildkite-agent artifact upload $bench_name.svg

  echo "+++ Heap profile"
  printf '\033]1338;url='"artifact://$bench_name.svg"';alt='"Heap profile"'\a\n'
fi
