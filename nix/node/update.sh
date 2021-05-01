#!/usr/bin/env bash

nix-shell -p nodePackages.node2nix -p jq --run \
  "node2nix -i $(dirname "$0")/node-packages.json \
  -o $(dirname "$0")/node-packages.nix \
  -c $(dirname "$0")/default.nix \
  -e $(dirname "$0")/node-env.nix"
