#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

forkpoint="$(git merge-base --fork-point main)"
changed_files="$(git diff --name-only "$forkpoint")"

# Filter out non-spec files
spec_files="$(echo "$changed_files" | grep '_spec.rb')"

# Match against spec files for modified implementation files
# non_spec_files=$(echo "$changed_files" | grep --invert-match '_spec.rb')

echo "Running tests for changed files:
$spec_files"

bundle exec rspec $spec_files
