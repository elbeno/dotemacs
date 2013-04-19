#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
emacs-w32 -q -l $DIR/../../.emacs $@
