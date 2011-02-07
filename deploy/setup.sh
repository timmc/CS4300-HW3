#!/bin/bash

set -o errexit
set -o nounset

cd "`dirname "$0"`"

curl --silent https://github.com/technomancy/leiningen/raw/stable/bin/lein > lein.sh
chmod +x lein.sh
./lein.sh uberjar

