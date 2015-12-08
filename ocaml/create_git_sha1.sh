#!/bin/bash

SHA1=$(git log -1  | head -1 | cut -d ' ' -f 2)
DATE=$(git log -1  | grep Date | cut -d ':' -f 2-)
MESSAGE=$(git log -1  | tail -1)
cat << EOF > Git.ml
open Core.Std
let sha1 = "$SHA1" |> String.strip
let date = "$DATE" |> String.strip
let message = "$MESSAGE" |> String.strip
EOF

