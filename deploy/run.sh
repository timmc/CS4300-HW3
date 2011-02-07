#!/bin/bash

set -o errexit
set -o nounset

cd "`dirname "$0"`"

java -jar timmc.HW3-standalone.jar "$@"

