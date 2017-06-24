#!/usr/bin/env bash

where="$(dirname $0)"
cd "$where"
npm run build

echo "static output in ${where}/build"


