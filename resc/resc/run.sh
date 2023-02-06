#!/usr/bin/env bash

set -e
set -x

for f in ~/resc/run/*; do
  $f
done

