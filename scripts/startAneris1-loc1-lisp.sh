ANERIS=..

DIR=${ANERIS}/examples
PWD=`pwd`
SBCL=sbcl
RUN_FASL_FILE=run.fasl

## configuration files
CONF_DF=${ANERIS}/conf/conf_aneris.emlc
CONF_LOC=${ANERIS}/conf/conf_aneris_local.emlc

CONF=${1:-${CONF_LOC}}

cd $DIR

#2/3 consensus
$SBCL --eval "(require 'asdf)" --eval "(require 'sb-bsd-sockets)" --eval "(require 'sb-posix)" --load "${RUN_FASL_FILE}" --eval "(test-loc1 \"${CONF}\")" &

cd $PWD
