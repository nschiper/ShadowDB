DIR="../examples"
SBCL=sbcl
RUN_FASL_FILE=run.fasl
RUN_FASL=${DIR}/${RUN_FASL_FILE}

#2/3 consensus
$SBCL --eval "(require 'asdf)" --eval "(require 'sb-bsd-sockets)" --eval "(require 'sb-posix)" --load "${RUN_FASL_FILE}" --eval "(test-loc1)" &

#Paxos acceptor
$SBCL --eval "(require 'asdf)" --eval "(require 'sb-bsd-sockets)" --eval "(require 'sb-posix)" --load "${RUN_FASL_FILE}" --eval "(test-acc1)" &

#Paxos leader
$SBCL --eval "(require 'asdf)" --eval "(require 'sb-bsd-sockets)" --eval "(require 'sb-posix)" --load "${RUN_FASL_FILE}" --eval "(test-ldr1)" &

#Replica
$SBCL --eval "(require 'asdf)" --eval "(require 'sb-bsd-sockets)" --eval "(require 'sb-posix)" --load "${RUN_FASL_FILE}" --eval "(test-rep1)" &
