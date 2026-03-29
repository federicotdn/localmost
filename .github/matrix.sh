#!/usr/bin/env bash
# Outputs the build matrix JSON for the release workflow.
# On tag pushes, includes all platforms. Otherwise, only Linux.

linux='{"os":"ubuntu-latest","name":"linux-x86_64","static":true}'
macos_x86='{"os":"macos-26-intel","name":"macos-x86_64","static":false}'
macos_arm='{"os":"macos-26","name":"macos-aarch64","static":false}'

if [[ "$GITHUB_REF" == refs/tags/* ]]; then
  echo "matrix={\"include\":[$linux,$macos_x86,$macos_arm]}" >> "$GITHUB_OUTPUT"
else
  echo "matrix={\"include\":[$linux]}" >> "$GITHUB_OUTPUT"
fi
