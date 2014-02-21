DIR="../examples"
SBCL=sbcl
RUN_LISP_FILE=run.lisp
SPEC_LISP_FILE=aneris-batching-control2.lisp
PWD=`pwd`

cd $DIR

echo "compiling Aneris"
$SBCL --load "prelude.lisp" --eval "(compile-file \"${SPEC_LISP_FILE}\")" --eval "(quit)"

echo "compiling ${RUN_LISP_FILE}"
$SBCL --eval "(require 'asdf)" --eval "(require 'cl-ppcre)" --eval "(require 'sb-bsd-sockets)" --eval "(require 'cl-lex)" --eval "(require 'marshal)" --eval "(require 'yacc)" --eval "(require 'ironclad)" --eval "(require 'flexi-streams)" --eval "(require 'sb-posix)" --load "prelude.lisp" --load "aneris-batching-control2.fasl" --eval "(compile-file \"${RUN_LISP_FILE}\")" --eval "(quit)"

cd ${PWD} 
