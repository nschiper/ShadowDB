DIR="../examples"
PWD=`pwd`
SBCL=sbcl
RUN_FASL_FILE=run.fasl

cd $DIR

#2/3 consensus
$SBCL --eval "(require 'asdf)" --eval "(require 'sb-bsd-sockets)" --eval "(require 'sb-posix)" --load "${RUN_FASL_FILE}" --eval "(test-loc4)" &

cd $PWD
