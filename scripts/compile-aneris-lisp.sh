DIR="../examples"
SBCL=sbcl
RUN_LISP_FILE=run.lisp
PWD=`pwd`

cd $DIR

echo "compiling Aneris"
$SBCL --load "prelude.lisp" --eval "(compile-file \"aneris_fastrep2_opt4_2lisp3.lisp\")" --eval "(quit)"

echo "compiling ${RUN_LISP_FILE}"
$SBCL --eval "(require 'asdf)" --eval "(require 'cl-ppcre)" --eval "(require 'sb-bsd-sockets)" --eval "(require 'cl-lex)" --eval "(require 'marshal)" --eval "(require 'yacc)" --eval "(require 'sb-posix)" --load "prelude.lisp" --load "aneris_fastrep2_opt4_2lisp3.fasl" --eval "(compile-file \"${RUN_LISP_FILE}\")" --eval "(quit)"

cd ${PWD} 
