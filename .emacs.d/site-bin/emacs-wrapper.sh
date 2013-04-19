#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
emacsclient-w32 -na $DIR/runemacs.sh `cygpath $1` ${*:2}
