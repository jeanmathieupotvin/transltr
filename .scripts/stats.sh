#!/bin/bash

# Create and/or update file STATISTICS.md
#
# @usage
# $ . .scripts/stats.sh
#
# @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#

input=STATISTICS.md
echo "Writing statistics to $input."

# Create file.
cloc . \
    --list-file=.clocinclude \
    --by-file \
    --out="$input" \
    --force-lang=R,Rprofile \
    --md \
    --hide-rate

# Replace first two lines included automatically by cloc.
sed -i \
    -e "1s/.*/# Statistics/" \
    -e "2d" \
    "$input"
