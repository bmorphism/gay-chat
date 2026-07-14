#!/bin/sh
set -eu

x=de
forbidden="${x}mo"

if grep -RIn --exclude-dir=.git --exclude='*.go' --exclude='*.wasm' -i "$forbidden" .; then
  echo 'found forbidden old framing language' >&2
  exit 1
fi
if grep -RIn --exclude-dir=.git --exclude='*.go' --exclude='*.wasm' -- "-${forbidden}" .; then
  echo 'found forbidden old framing token' >&2
  exit 1
fi
if find . -path ./.git -prune -o -name "*${forbidden}*" -print | grep .; then
  echo 'found forbidden old framing filename' >&2
  exit 1
fi
printf 'world-language-test ok\n'
