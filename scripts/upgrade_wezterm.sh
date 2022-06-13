#!/bin/bash

set -x
set -e

REPO_PATH=~/repos/wezterm
cd $REPO_PATH

git pull

git submodule update --init --recursive
./get-deps


export MACOSX_DEPLOYMENT_TARGET=10.9
cargo build --target x86_64-apple-darwin --all --release


builddir=WezTerm-macos-nightly

rm -rf $builddir
mkdir $builddir
cp -r assets/macos/WezTerm.app $builddir/
# Omit MetalANGLE for now; it's a bit laggy compared to CGL,
# and on M1/Big Sur, CGL is implemented in terms of Metal anyway
rm -rf $builddir/WezTerm.app/*
mkdir -p $builddir/WezTerm.app/Contents/MacOS
mkdir -p $builddir/WezTerm.app/Contents/Resources
cp -r assets/shell-integration/* $builddir/WezTerm.app/Contents/Resources

for bin in wezterm wezterm-mux-server wezterm-gui strip-ansi-escapes ; do
  # If the user ran a simple `cargo build --release`, then we want to allow
  # a single-arch package to be built
  if [[ -f target/release/$bin ]] ; then
    cp target/release/$bin $builddir/WezTerm.app/Contents/MacOS/$bin
  else
    # The CI runs `cargo build --target XXX --release` which means that
    # the binaries will be deployed in `target/XXX/release` instead of
    # the plain path above.
    # In that situation, we have two architectures to assemble into a
    # Universal ("fat") binary, so we use the `lipo` tool for that.
    lipo target/*/release/$bin -output $builddir/WezTerm.app/Contents/MacOS/$bin -create
  fi
done

cp -pR $builddir/WezTerm.app/* /Applications/Wezterm.app/
