#!/bin/bash

# Check if an argument is provided
if [ $# -ne 1 ]; then
    echo "Usage: $0 ./test/<FILENAME>.l1"
    exit 1
fi

INPUT="$1"

# Ensure the input matches the expected pattern
if [[ "$INPUT" != ./test/*.l1 ]]; then
    echo "Error: INPUT must be of the form ./test/<FILENAME>.l1"
    exit 1
fi

# Extract the base filename (without path and .l1 extension)
FILENAME=$(basename "$INPUT" .l1)
BINARY="./out/$FILENAME"

# Run the program with cargo
RUST_BACKTRACE=1 cargo run "$INPUT"
CARGO_EXIT=$?

# Check if cargo run was successful
if [ $CARGO_EXIT -eq 0 ]; then
    # Check if the expected binary exists and is executable
    if [ -x "$BINARY" ]; then
        "$BINARY"
        echo "Binary exit code: $?"
    else
        echo "Error: Expected binary '$BINARY' not found or not executable."
        exit 2
    fi
else
    echo "Error: cargo run failed with exit code $CARGO_EXIT"
    exit $CARGO_EXIT
fi
