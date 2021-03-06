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

#Paxos leader
$SBCL --eval "(require 'asdf)" --eval "(require 'sb-bsd-sockets)" --eval "(require 'sb-posix)" --load "${RUN_FASL_FILE}" --eval "(test-ldr2 \"${CONF}\")" &

cd $PWD
