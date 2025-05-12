#!/bin/bash
set -e
cargo build --release
cp target/release/comp-design-ss25 ./comp-design-ss25