#!/bin/bash

set -Eeuo pipefail

EXIT=0
for file in src/*.fsx
do
  dotnet fsi $file || EXIT=1
done
exit $EXIT
