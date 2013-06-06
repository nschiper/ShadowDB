ANERIS=..

DIR=${ANERIS}/examples
PWD=`pwd`
SBCL=sbcl
RUN_FASL_FILE=run.fasl

## configuration files
CONF_DF=${ANERIS}/conf/conf_aneris.emlc
CONF_LOC=${ANERIS}/conf/conf_aneris_local.emlc
CONF_PB=${ANERIS}/conf/conf_aneris_pb.emlc
CONF_TB=${ANERIS}/conf/conf_aneris_tobcast.emlc
CONF_SMR=${ANERIS}/conf/conf_aneris_smr_lisp.emlc

CONF=${CONF_LOC}

cd $DIR

#2/3 consensus
$SBCL --eval "(require 'asdf)" --eval "(require 'sb-bsd-sockets)" --eval "(require 'sb-posix)" --load "${RUN_FASL_FILE}" --eval "(test-loc4 \"${CONF}\")" &

cd $PWD
