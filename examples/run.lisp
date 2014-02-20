;; Copyright 2011 Cornell University
;; Copyright 2012 Cornell University
;; Copyright 2013 Cornell University
;;
;;
;; This file is part of EventML - a tool aiming at specifying
;; distributed protocols in an ML like language.  It is an interface
;; to the logic of events and is compiled into Nuprl.  It is written
;; by the NUPRL group of Cornell University, Ithaca, NY.
;;
;; EventML is a free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; EventML is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EventML.  If not, see <http://www.gnu.org/licenses/>.
;;
;;  o Authors:     Vincent Rahli
;;  o Affiliation: Cornell University, NUPRL group
;;  o Date:        10 May 2013
;;  o File name:   run.lisp
;;  o Description: Lisp environment for Nuprl processes


;;;;;; LOAD THE PRELUDE

(load "prelude.lisp")


;;;;;; LOAD THE LEXER AND PARSER GENERATORS

;; I've installed these packages using cl-quicklisp
;;    o Download quicklisp.lisp here: http://www.quicklisp.org/beta/
;;    o run in sbcl: (load "quicklisp.lisp")
;;    o then: (quicklisp-quickstart:install)
;;    o then: (ql:quickload "cl-lex")
;;    o then: (ql:quickload "yacc")
;;    o then: (ql:quickload "marshal")
;;    o then: (ql:quickload "ironclad")
;;    o then: (ql:quickload "flexi-streams")
;;    o then: (ql:add-to-init-file)
;;

(require :cl-ppcre)
(require :cl-lex)
(require :marshal)
(require :yacc)
(require :ironclad)
(require :flexi-streams)

;;(load "yacc.fasl")
;;(use-package '#:yacc)
;;(load "packages.lisp")
;;(load "lex.lisp")
;;(use-package :cl-lex)

;;(defun main () nil)


;;;; --------- SWITCHES ---------

;; true to use the stream read/write and false to use the socket send/receive
(defvar new-send-receive nil)

;; true to get the client to log its inputs
(defvar eml-debug nil)

;; true to get a move call-by-name-ish program
;; The spec also has to change in run-lisp.sh
;;
;; This should be used according to the way the Lisp spec file has
;; been generated (see the comments containing "call-by-name" in
;; NuprlTerms.sml).
(defvar cbn t)

;; Delayed Message Handler (dmh) has to be true to use that
;; functionality which is that we create a process that handles
;; delayed messages right from the start instead of creating new
;; threads each time we have to send a new delayed message.
(defvar dmh t)


;; The 'auth' variable has to be true to sign and verify messages.
(defvar auth nil)


;;;; --------- VARIABLES ---------

(defvar nuprl-msgs nil)
(defvar delayed-nuprl-msgs nil)

;; public and private keys are of type (VECTOR (UNSIGNED-BYTE 8))
(defvar pbkey nil)
(defvar prkey nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;      STRUCTURES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct location
  id
  host
  port)
(defstruct group
  name
  member
  locations)
(defstruct connection
  from
  to)
(defstruct parameter
  name
  value)
(defstruct intransit
  id
  header
  type
  expression)
(defstruct conf
  groups
  connections
  parameters
  intransit)

(defstruct nfo
  id
  host
  port
  name
  member
  socket
  desc
  stream)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;      PARSER OF CONFIGURATION FILES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun read-section (stream)
  (let ((lines nil))
    (do ((line (read-line stream nil)
	       (read-line stream nil)))
	((or (null line)
	     (string= line "")))
      (setq lines (append lines (cons line nil)))
      )
    lines
    )
  )

;; checks whether a section of a configuration file is a %locations section
(defun is-section-group (section)
  (string= "%locations" (car (ppcre:split "\\s+" (car section))))
  )

;; checks whether a section of a configuration file is a %connections section
(defun is-section-connections (section)
  (string= "%connections" (car (ppcre:split "\\s+" (car section))))
  )

;; checks whether a section of a configuration file is a %parameters section
(defun is-section-parameters (section)
  (string= "%parameters" (car (ppcre:split "\\s+" (car section))))
  )

;; checks whether a section of a configuration file is a %messages section
(defun is-section-intransit (section)
  (string= "%messages" (car (ppcre:split "\\s+" (car section))))
  )

;; checks whether a %locations section is external or internal
(defun is-member-group (header)
  (not (string= "external" (car (cdr (ppcre:split "\\s+" header)))))
  )

;; gets the name of group given a header of the form:
;;   %locations foo
;; or of the form:
;;   %locations external foo
(defun get-group-name (header)
  (let ((lst (ppcre:split "\\s+" header)))
    (if (string= "external" (car (cdr lst)))
	(car (cdr (cdr lst)))
	(car (cdr lst))
	)
    )
  )

;; extracts the member of a list of group members (from string to structure)
(defun get-group-members (lst)
  (if lst
      (multiple-value-bind (match strings)
	  (ppcre:scan-to-strings "[ ]*([^: ]+)[ ]*:[ ]*([^ ]+)[ ]*([1-9]+)"
				 (car lst))
	(declare (ignore match))
	(cons (make-location :id   (aref strings 0)
			     :host (aref strings 1)
			     :port (aref strings 2))
	      (get-group-members (cdr lst))
	      )
	)
      nil
      )
  )

;; parses a group
(defun parse-group (group)
  ;;(print (format nil "parsing group ~A" group))
  (let* ((header    (car group))
	 (locations (get-group-members (cdr group)))
	 (name      (get-group-name header))
	 (member    (is-member-group header)))
    (make-group :name name
		:member member
		:locations locations)
    )
  )

(defun parse-connections (connections)
  (mapcar (lambda (connection)
	    (multiple-value-bind (match strings)
		(ppcre:scan-to-strings "[ ]*([^ ]+)[ ]*->[ ]*([^ ]+)[ ]*"
				       connection)
	      (declare (ignore match))
	      (make-connection :from (aref strings 0)
			       :to   (aref strings 1))
	      )
	    )
	  (cdr connections)
	  )
  )

;; converts and EML location (of the form LOC(loc)) to a lisp one (a string)
(defun parse-eml-loc (exp)
  ;;(print (format nil "parsing location ~A" exp))
  (multiple-value-bind (match strings)
      (ppcre:scan-to-strings "[ ]*LOC[ ]*\\\([ ]*([^\) ]+)[ ]*\\\)[ ]*"
			     exp)
    ;;(print (format nil "match ~A" match))
    (if (string= match exp)
	(aref strings 0)
	nil)
    )
  )

(defun parse-eml-tokens (exp)
  (multiple-value-bind (match strings)
      (ppcre:scan-to-strings "[ ]*``([^`]+)``[ ]*"
			     exp)
    (if (string= match exp)
	(aref strings 0)
	nil)
    )
  )

(defun parse-eml-bag-members (exp parse)
  (print (format nil "parsing bag members ~A" exp))
  (multiple-value-bind (match strings)
      (ppcre:scan-to-strings "[ ]*([^;]+);(.*)"
			     exp)
    (if (string= match exp)
	(make-pair :fst (funcall parse (aref strings 0))
		   :snd (parse-eml-bag-members (aref strings 1) parse))
	(if (ppcre:split "\\s+" exp)
	    (make-pair :fst (funcall parse exp) :snd (make-axiom))
	    (make-axiom)
	    )
	)
    )
  )

(defun parse-eml-bag (exp parse)
  (multiple-value-bind (match strings)
      (ppcre:scan-to-strings "[ ]*{[ ]*([^}]+)[ ]*}[ ]*"
			     exp)
    (if (string= match exp)
	(parse-eml-bag-members (aref strings 0) parse)
	nil)
    )
  )

(defun parse-parameter (parameter)
  (let ((exp (parse-eml-loc parameter)))
    (if exp
	exp
	(let ((exp (parse-eml-tokens parameter)))
	  (if exp
	      exp
	      (let ((exp (parse-eml-bag parameter #'parse-parameter)))
		(if exp
		    exp
		    "-")
		)
	      )
	  )
	)
    )
  )

(defun parse-parameters (parameters)
  (mapcar (lambda (parameter)
	    (multiple-value-bind (match strings)
		(ppcre:scan-to-strings "[ ]*([^: ]+)[ ]*:[ ]*(.*)"
				       parameter)
	      (declare (ignore match))
	      (make-parameter :name  (aref strings 0)
			      :value (parse-parameter (aref strings 1)))
	      )
	    )
	  (cdr parameters)
	  )
  )

(defun parse-sections (stream groups connections parameters intransit)
  (let ((section (read-section stream)))
    ;;(print (format nil "checking kind of section ~A" section))
    (if (is-section-group section)
	(parse-sections stream
			(append groups (cons (parse-group section) nil))
			connections
			parameters
			intransit)
	(if (is-section-connections section)
	    (parse-sections stream
			    groups
			    (append connections (parse-connections section))
			    parameters
			    intransit)
	    (if (is-section-parameters section)
		(parse-sections stream
				groups
				connections
				(append parameters (parse-parameters section))
				intransit)
		(make-conf :groups groups
			   :connections connections
			   :parameters parameters
			   :intransit intransit)
		)
	    )
	)
    )
  )

(defun parse-configuration-file (file)
  (with-open-file (stream file :if-does-not-exist nil)
    (let ((groups      nil)
	  (connections nil)
	  (parameters  nil)
	  (intransit   nil))
      (parse-sections stream groups connections parameters intransit)
      )
    )
  )

(defun file-to-string (file)
  (let ((str nil))
    (with-open-file (stream file :if-does-not-exist nil)
      (do ((line (read-line stream nil)
		 (read-line stream nil)))
	  ((null line))
	(setq str (concatenate 'string str line))
	)
      )
    str
    )
  )

(defun file-string (file)
  (with-open-file (s file)
    (let* ((len (file-length s))
           (data (make-string len)))
      (read-sequence data s))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;      BETTER PARSER OF CONFIGURATION FILES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mk-arg (a)
  (if cbn
      (lambda () a)
      a
      )
  )

(defun mk-call (a)
  (if cbn
      (funcall a)
      a
      )
  )

(defun mk-app (f a)
  (if cbn
      (funcall f (mk-arg a))
      (funcall f a)
      )
  )


(defun 2-nuprl-bool (b)
  (if b
      (make-inl :val (make-axiom))
      (make-inr :val (make-axiom))
      )
  )

(defun band (nb1 nb2)
  (2-nuprl-bool (and (inl-p nb1) (inl-p nb2)))
  )

(defun list-deq (eq)
  (lambda (list1)
    (lambda (list2)
      (let ((list1 (mk-call list1))
	    (list2 (mk-call list2)))
	(if (pair-p list1)
	    (band (2-nuprl-bool (pair-p list2))
		  (band (funcall (funcall eq
					  (pair-fst list1))
				 (pair-fst list2))
			(funcall (funcall (list-deq eq)
					  (mk-arg (pair-snd list1)))
				 (mk-arg (pair-snd list2)))
			)
		  )
	    (if (axiom-p list1)
		(2-nuprl-bool (axiom-p list2))
		(progn
		  (print (format nil "list1: ~A" list1))
		  (print (format nil "list1: ~A" list2))
		  (error "1st list is not a list")
		  )
		)
	    )
	)
      )
    )
  )

(defun product-deq (eq1 eq2)
  (lambda (p1)
    (lambda (p2)
      (if (and (pair-p p1) (pair-p p2))
	  (band (funcall (funcall eq1 (pair-fst p1)) (pair-fst p2))
		(funcall (funcall eq2 (pair-snd p1)) (pair-snd p2))
		)
	  (error "product-deq:not-pairs")
	  )
      )
    )
  )

(defun int-deq ()
  (lambda (n1)
    (lambda (n2)
      (2-nuprl-bool (= n1 n2))
      )
    )
  )

(defun tok-deq ()
  (lambda (tok1)
    (lambda (tok2)
      (2-nuprl-bool (string= tok1 tok2))
      )
    )
  )

;; Parser (lexer is defined below)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-build-list-0 ()
    nil
    )

  (defun parse-build-list-1 (x)
    ;;(print "parse-build-list-1")
    (cons x nil)
    )

  (defun parse-build-list-2 (x xs)
    ;;(print "parse-build-list-2")
    (cons x xs)
    )

  (defun parse-group-1 (locations id locs)
    (declare (ignore locations))
    ;;(print id)
    (make-group :name id
		:member t
		:locations locs)
    )

  (defun parse-group-2 (locations external id locs)
    (declare (ignore locations))
    (declare (ignore external))
    ;;(print id)
    (make-group :name id
		:member nil
		:locations locs)
    )

  (defun parse-location-1 (id colon ip numeral)
    (declare (ignore colon))
    (make-location :id id
		   :host ip
		   :port numeral)
    )

  (defun parse-id (x)
    x
    )

  (defun parse-ip-2 (numeral1 dot1 numeral2 dot2 numeral3 dot3 numeral4)
    (declare (ignore dot1))
    (declare (ignore dot2))
    (declare (ignore dot3))
    (concatenate 'string
		 (write-to-string numeral1)
		 "."
		 (write-to-string numeral2)
		 "."
		 (write-to-string numeral3)
		 "."
		 (write-to-string numeral4))
    )

  (defun parse-numeral-1 (n)
    (parse-integer n)
    )

  (defun parse-connections-1 (connections conns)
    (declare (ignore connections))
    conns
    )

  (defun parse-conn-1 (id1 arrow id2)
    (declare (ignore arrow))
    (make-connection :from id1
		     :to   id2)
    )

  (defun parse-parameters-1 (parameters params)
    (declare (ignore parameters))
    params
    )

  (defun parse-messages-1 (messages msgs)
    (declare (ignore messages))
    msgs
    )

  (defun parse-param-1 (id colon exp)
    (declare (ignore colon))
    (make-parameter :name  id
		    :value exp)
    )

  (defun parse-expemenv-1 (exp)
    (funcall exp nil)
    )

  (defun parse-param-2 (id colon type lparen typ rparen)
    (declare (ignore colon))
    (declare (ignore type))
    (declare (ignore lparen))
    (declare (ignore rparen))
    (make-parameter :name  id
		    :value typ)
    )

  (defun parse-param-3 (id colon deq lparen eqdec rparen)
    (declare (ignore colon))
    (declare (ignore deq))
    (declare (ignore lparen))
    (declare (ignore rparen))
    (make-parameter :name  id
		    :value eqdec)
    )

  (defun parse-typ-1 (atty id)
    (if (string= id "List")
	(concatenate 'string "list(" atty ")")
	(error "parse-typ-1:unexpected type")
	)
    )

  (defun parse-typ-2 (typ1 star typ2)
    (declare (ignore star))
    (concatenate 'string "product(" typ1 ",," typ2 ")")
    )

  (defun parse-atty-1 (id)
    (if (string= id "Tok")
	"token"
	(if (string= id "Int")
	    "int"
	    (if (string= id "Loc")
		"loc"
		(error (concatenate 'string "parse-atty-1:unexpected type" id))
		)
	    )
	)
    )

  (defun parse-atty-2 (lparen typ rparen)
    (declare (ignore lparen))
    (declare (ignore rparen))
    typ
    )

  (defun parse-eqdec-1 (speqdec id)
    (if (string= id "List")
	(list-deq speqdec)
	(error "parse-eqdec-1:unexpected type")
	)
    )

  (defun parse-eqdec-2 (eqdec1 star eqdec2)
    (declare (ignore star))
    (product-deq eqdec1 eqdec2)
    )

  (defun parse-speqdec-1 (id)
    (if (string= id "Tok")
	(tok-deq)
	(if (string= id "Int")
	    (int-deq)
	    (if (string= id "Loc")
		(tok-deq)
		(error (concatenate 'string "parse-speqdec-1:unexpected-type-" id))
		)
	    )
	)
    )

  (defun parse-speqdec-2 (lparen eqdec rparen)
    (declare (ignore lparen))
    (declare (ignore rparen))
    eqdec
    )

  (defun parse-exp-list (ldel explist rdel)
    (declare (ignore ldel))
    (declare (ignore rdel))
    (lambda (lst) (funcall explist lst))
    )

  (defun parse-exp-lam (lam id dot exp)
    (declare (ignore lam))
    (declare (ignore dot))
    (lambda (lst) (lambda (x) (funcall exp (acons id x lst))))
    )

  (defun parse-exp-ite (iteif exp1 itethen exp2 iteelse exp3)
    (declare (ignore iteif))
    (declare (ignore itethen))
    (declare (ignore iteelse))
    (lambda (lst)
      (if (funcall exp1 lst)
	  (funcall exp2 lst)
	  (funcall exp3 lst)
	  )
      )
    )

  (defun parse-exp-pair (lparen exp1 comma exp2 rparen)
    (declare (ignore lparen))
    (declare (ignore comma))
    (declare (ignore rparen))
    (lambda (lst)
      (make-pair :fst (funcall exp1 lst)
		 :snd (funcall exp2 lst)
		 )
      )
    )

  (defun parse-expeq-1 (id equal exploc)
    (declare (ignore equal))
    (lambda (lst) (string=
		   (mk-call (cdr (assoc id lst :test #'string=)))
		   (funcall exploc lst)
		   )
	    )
    )

  (defun parse-expint-1 (n)
    (lambda (lst)
      (declare (ignore lst))
      n)
    )

  (defun parse-explist-0 ()
    (lambda (lst)
      (declare (ignore lst))
      (make-axiom))
    )

  (defun parse-explist-1 (exp)
    (lambda (lst) (make-pair :fst (funcall exp lst) :snd (make-axiom)))
    )

  (defun parse-explist-2 (exp semicolon explist)
    (declare (ignore semicolon))
    (lambda (lst) (make-pair :fst (funcall exp lst) :snd (funcall explist lst)))
    )

  (defun parse-pair-1 (x)
    (make-pair :fst x :snd (make-axiom))
    )

  (defun parse-pair-2 (x y)
    (make-pair :fst x :snd y)
    )

  (defun parse-exploc-1 (loc lparen id rparen)
    (declare (ignore loc))
    (declare (ignore lparen))
    (declare (ignore rparen))
    (lambda (lst)
      (declare (ignore lst))
      id)
    )

  (defun parse-atoms-1 (bq1 bq2 atomlist bq3 bq4)
    (declare (ignore bq1))
    (declare (ignore bq2))
    (declare (ignore bq3))
    (declare (ignore bq4))
    (lambda (lst)
      (declare (ignore lst))
      atomlist)
    )

  (defun parse-msg-1 (id colon lparen atoms comma1 typ comma2 exp rparen)
    (declare (ignore colon))
    (declare (ignore lparen))
    (declare (ignore comma1))
    (declare (ignore comma2))
    (declare (ignore rparen))
    (make-intransit :id id
		    :header atoms
		    :type typ
		    :expression exp)
    )

  (defun parse-conf-1 (conf_groups conf_conns conf_params conf_msgs)
    (make-conf :groups      conf_groups
	       :connections conf_conns
	       :parameters  conf_params
	       :intransit   conf_msgs
	       )
    )
  )

(yacc:define-parser conf-parser
  (:start-symbol conf)
  (:terminals (:dot
	       :comma
	       :int
	       :num
	       :id
	       :arrow
	       :lparen
	       :rparen
	       :semicolon
	       :lbrace
	       :rbrace
	       :equal
	       :iteif
	       :itethen
	       :iteelse
	       :type
	       :deq
	       :loc
	       :lam
	       :backquote
	       :locations
	       :connections
	       :messages
	       :parameters
	       :external
	       :star
	       :colon))
  (:precedence ((:left  :comma)
		(:right :arrow)
		(:left  :equal)
		(:left  :semicolon)
		(:left  :star)
		(:left  :colon)))

  (conf
   (conf_groups conf_conns conf_params conf_msgs #'parse-conf-1)
   )

  (conf_groups
   (conf_group             #'parse-build-list-1)
   (conf_group conf_groups #'parse-build-list-2)
   )

  (conf_group
   (:locations :id locs           #'parse-group-1)
   (:locations :external :id locs #'parse-group-2)
   )

  (locs
   (location      #'parse-build-list-1)
   (location locs #'parse-build-list-2)
   )

  (location
   (:id :colon ip numeral #'parse-location-1)
   )

  (ip
   (:id                                            #'parse-id)
   (numeral :dot numeral :dot numeral :dot numeral #'parse-ip-2)
   )

  (numeral
   (:num #'parse-numeral-1)
   (:int #'parse-numeral-1)
   )

  (conf_conns
   (:connections conns #'parse-connections-1)
   )

  (conns
   (conn       #'parse-build-list-1)
   (conn conns #'parse-build-list-2)
   )

  (conn
   (:id :arrow :id #'parse-conn-1)
   )

  (conf_params
   (:parameters params #'parse-parameters-1)
   )

  (params
   (             #'parse-build-list-0)
   (param params #'parse-build-list-2)
   )

  (param
   (:id :colon expemenv                   #'parse-param-1)
   (:id :colon :type :lparen typ :rparen  #'parse-param-2)
   (:id :colon :deq :lparen eqdec :rparen #'parse-param-3)
   )

  (expemenv
   (exp #'parse-expemenv-1)
   )

  (exp
   (exploc                               #'parse-id)
   (expeq                                #'parse-id)
   (expint                               #'parse-id)
   (atoms                                #'parse-id)
   (:lbrace explist :rbrace              #'parse-exp-list)
   (:lam :id :dot exp                    #'parse-exp-lam)
   (:iteif exp :itethen exp :iteelse exp #'parse-exp-ite)
   (:lparen exp :comma exp :rparen       #'parse-exp-pair)
   )

  (atoms
   (:backquote :backquote atomlist :backquote :backquote #'parse-atoms-1)
   )

  (atomlist
   (:id          #'parse-pair-1)
   (:id atomlist #'parse-pair-2)
   )

  (expint
   (numeral #'parse-expint-1)
   )

  (expeq
   (:id :equal exploc #'parse-expeq-1)
   )

  (explist
   (                       #'parse-explist-0)
   (exp                    #'parse-explist-1)
   (exp :semicolon explist #'parse-explist-2)
   )

  (exploc
   (:loc :lparen :id :rparen #'parse-exploc-1)
   )

  (typ
   (atty          #'parse-id)
   (atty :id      #'parse-typ-1)
   (typ :star typ #'parse-typ-2)
   )

  (atty
   (:id                 #'parse-atty-1)
   (:lparen typ :rparen #'parse-atty-2)
   )

  (eqdec
   (speqdec           #'parse-id)
   (speqdec :id       #'parse-eqdec-1)
   (eqdec :star eqdec #'parse-eqdec-2)
   )

  (speqdec
   (:id                   #'parse-speqdec-1)
   (:lparen eqdec :rparen #'parse-speqdec-2)
   )

  (conf_msgs
   (:messages msgs #'parse-messages-1)
   )

  (msgs
   (         #'parse-build-list-0)
   (msg msgs #'parse-build-list-2)
   )

  (msg
   (:id :colon :lparen atoms :comma typ :comma exp :rparen #'parse-msg-1)
   )
  )

;; Lexer (parser is defined above)
(cl-lex:define-string-lexer conf-lexer
    ;;("\\%\\%[^\\n]*\\n")
    ("[ \\t\\n]")
    ("%locations"            (return (values :locations   $@)))
    ("%connections"          (return (values :connections $@)))
    ("%parameters"           (return (values :parameters  $@)))
    ("%messages"             (return (values :messages    $@)))
    ("%database"             (return nil))
    ("in"                    (return (values :in          $@)))
    ("external"              (return (values :external    $@)))
    ("if"                    (return (values :iteif       $@)))
    ("then"                  (return (values :itethen     $@)))
    ("else"                  (return (values :iteelse     $@)))
    ("LOC"                   (return (values :loc         $@)))
    ("TYPE"                  (return (values :type        $@)))
    ("DEQ"                   (return (values :deq         $@)))
    ("[a-zA-Z][a-zA-Z0-9_]*" (return (values :id          $@)))
    ("\\\\"                  (return (values :lam         $@)))
    ("\\("                   (return (values :lparen      $@)))
    ("\\)"                   (return (values :rparen      $@)))
    ("\\{"                   (return (values :lbrace      $@)))
    ("\\}"                   (return (values :rbrace      $@)))
    ("\\:"                   (return (values :colon       $@)))
    ("\\;"                   (return (values :semicolon   $@)))
    ("\\,"                   (return (values :comma       $@)))
    ("\\`"                   (return (values :backquote   $@)))
    ("\\."                   (return (values :dot         $@)))
    ("\\="                   (return (values :equal       $@)))
    ("\\*"                   (return (values :star        $@)))
    ("\\-\\>"                (return (values :arrow       $@)))
    ("[0-9]+"                (return (values :int         $@)))
  )

     ;; (defun list-lexer (list)
     ;;   #'(lambda ()
     ;;       (let ((value (pop list)))
     ;;         (if (null value)
     ;;             (values nil nil)
     ;;             (let ((terminal
     ;;                    (cond ((member value '(+ - * / |(| |)|)) value)
     ;;                          ((integerp value) 'int)
     ;;                          ((symbolp value) 'id)
     ;;                          (t (error "Unexpected value ~S" value)))))
     ;;               (values terminal value))))))


;; Parses the configuration file `file` and generates a `conf` structure
(defun parse-conf-file (file)
  (let ((str (file-to-string file)))
    ;; (print str)
    (yacc:parse-with-lexer (conf-lexer str) conf-parser)
    )
  )

(defun find-param (param params)
  (if params
      (let ((x (car params)))
	(if (string= param (parameter-name x))
	    x
	    (find-param param (cdr params))
	    )
	)
      (error "find-param:could not find parameter")
      )
  )

(defun test-parser (param loc)
  (let* ((conf   (parse-conf-file "conf_aneris_batching.emlc"))
	 (params (conf-parameters conf))
	 (param  (find-param param params)))
    (funcall (parameter-value param) (mk-arg loc))
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;      SIGNATURES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Uses DSA

;; Uses private-key 'prkey' to sign message
(defun mk-sign (message)
  ;; returns a signature
  (ironclad:sign-message prkey message)
  )

;; Uses public-key 'pbkey' to verify message
(defun verify (message signature)
  ;; returns a Boolean
  (ironclad:verify-signature pbkey message signature)
  )

(defun print-public-key (key)
  (let ((p (ironclad:dsa-key-p key))
	(q (ironclad:dsa-key-q key))
	(g (ironclad:dsa-key-g key))
	(y (ironclad:dsa-key-y key)))
    (flexi-streams:octets-to-string (cons p (cons q (cons g (cons y nil)))) :external-format :utf-8)
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;      SOCKETS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun print-eml-loc (loc str)
  (format t (concatenate 'string "[" loc ":" str "]~%"))
  (finish-output)
  )

(defun new-socket ()
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			       :type :stream
			       :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-tcp-nodelay socket) t)
    ;;(setf (sb-bsd-sockets:non-blocking-mode socket) t)
    socket
    )
  )

(defun xxxx-new-socket ()
  (make-instance 'sb-bsd-sockets:inet-socket
		 :type :stream
		 :protocol :tcp)
  )

;; creates a server sockets associated to port 'port'
;; 'loc' is the location that creates the server and 'ip' its ip
(defun create-server-socket (loc ip port)
  (print-eml-loc loc "creating server socket")
  (let ((socket (new-socket))
	(addr (sb-bsd-sockets:make-inet-address ip)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (print-eml-loc loc (format nil "binding[~A;~A;~A]..." ip addr port))
    (sb-bsd-sockets:socket-bind socket addr port)
    (print-eml-loc loc "binding done")
    (print-eml-loc loc "listening")
    (sb-bsd-sockets:socket-listen socket 100)
    socket
    )
  )

;; extract the location in the location lists locations
;; that has the ip and port passed as arguments
(defun get-location-from-list (ip port locations)
  (if locations
      (let ((loc (car locations)))
	(if (and (string= ip (location-host loc))
		 (= port (location-port loc)))
	    loc
	    (get-location-from-list ip port (cdr locations))
	    )
	)
      nil
      )
  )

;; find the group in the group list groups that contains a location
;; that has ip and port passed as arguments
(defun find-group (ip port groups)
  (if groups
      (let ((group (car groups)))
	(if (get-location-from-list ip port (group-locations group))
	    group
	    (find-group ip port (cdr groups))
	    )
	)
      nil
      )
  )

(defun get-sp-internal-locs-from-groups (groups)
  (reduce (lambda (locations group)
	    (if (group-member group)
		(append (group-locations group) locations)
		locations
		)
	    )
	  groups
	  :initial-value nil
	  )
  )

(defun get-sp-locs-from-groups (groups)
  (reduce (lambda (locations group) (append (group-locations group) locations))
	  groups
	  :initial-value nil
	  )
  )

(defun get-id-in-locs (ident locations)
  (find ident locations :test #'(lambda (x y) (string= x (location-id y))))
  )

(defun get-id-in-groups (ident groups)
  (get-id-in-locs ident (get-sp-locs-from-groups groups))
  )

(defun find-group-id (id groups)
  (find id groups :test #'(lambda (id group) (get-id-in-locs id (group-locations group))))
  )

;; get the locations from the list of locations that have an ip/port
;; less than the ip/port passed as arguments
(defun get-lt-locations-in-group (ip port name member locations)
  (if locations
      (let* ((loc  (car locations))
	     (i    (location-id   loc))
	     (h    (location-host loc))
	     (p    (location-port loc))
	     (rest (get-lt-locations-in-group ip port name member (cdr locations)))
	     (nfo  (make-nfo :id i :host h :port p :name name :member member)))
	(if (string< ip h)
	    rest
	    (if (string= ip h)
		(if (< port p)
		    rest
		    (if (= port p)
			rest
			(cons nfo rest)
			)
		    )
		(cons nfo rest)
		)
	    )
	)
      nil
      )
  )

;; get the locations from the list of locations that have an ip/port
;; greater than the ip/port passed as arguments
(defun get-gt-locations-in-group (ip port name member locations)
  (if locations
      (let* ((loc  (car locations))
	     (i    (location-id   loc))
	     (h    (location-host loc))
	     (p    (location-port loc))
	     (rest (get-gt-locations-in-group ip port name member (cdr locations)))
	     (nfo  (make-nfo :id i :host h :port p :name name :member member)))
	(if (string< ip h)
	    (cons nfo rest)
	    (if (string= ip h)
		(if (< port p)
		    (cons nfo rest)
		    rest
		    )
		rest
		)
	    )
	)
      nil
      )
  )

(defun find-gt-groups (name connections)
  (if connections
      (let ((conn (car connections)))
	(if (string= name (connection-from conn))
	    (cons (connection-to conn) (find-gt-groups name (cdr connections)))
	    (find-gt-groups name (cdr connections))
	    )
	)
      nil
      )
  )

(defun find-lt-groups (name connections)
  (if connections
      (let ((conn (car connections)))
	(if (string= name (connection-to conn))
	    (cons (connection-from conn) (find-lt-groups name (cdr connections)))
	    (find-lt-groups name (cdr connections))
	    )
	)
      nil
      )
  )

(defun filter-groups (groups names)
  (if groups
      (let ((group (car groups)))
	(if (find (group-name group) names :test #'string=)
	    (cons group (filter-groups (cdr groups) names))
	    (filter-groups (cdr groups) names)
	    )
	)
      nil
      )
  )

(defun get-locs-from-groups (groups)
  (if groups
      (append (group-locations (car groups)) (get-locs-from-groups (cdr groups)))
      nil
      )
  )

(defun get-nfos-from-groups (groups)
  (if groups
      (let* ((group     (car groups))
	     (name      (group-name      group))
	     (member    (group-member    group))
	     (locations (group-locations group))
	     (nfos      (get-nfos-from-groups (cdr groups))))
	(append (mapcar (lambda (loc) (make-nfo :id     (location-id   loc)
						:host   (location-host loc)
						:port   (location-port loc)
						:name   name
						:member member))
			locations)
		nfos)
	)
      nil
      )
  )

(defun get-lt-locations (loc ip port conf)
  ;; group is the group in which (ip,port) is
  (let* ((groups (conf-groups conf))
	 (group  (find-group-id loc groups)))
    (if group
	(let* ((name      (group-name      group))
	       (member    (group-member    group))
	       (locations (group-locations group))
	       (lt (get-lt-locations-in-group ip port name member locations))
	       (ltgroups (find-lt-groups name (conf-connections conf)))
	       (locs (get-nfos-from-groups (filter-groups groups ltgroups))))
	  (if member
	      (append lt locs)
	      (if ltgroups
		  (error "get-lt-locations:not-member-is-gt-group")
		  nil ;; we dont return lt because loc is not part of the system
		  )
	      )
	  )
	nil
	)
    )
  )

(defun get-gt-locations (loc ip port conf)
  ;; group is the group in which (ip,port) is
  (let* ((groups (conf-groups conf))
	 (group  (find-group-id loc groups)))
    (if group
	(let* ((name      (group-name      group))
	       (member    (group-member    group))
	       (locations (group-locations group))
	       (gt (get-gt-locations-in-group ip port name member locations))
	       (gtgroups (find-gt-groups name (conf-connections conf)))
	       (locs (get-nfos-from-groups (filter-groups groups gtgroups))))
	  (if member
	      (append gt locs)
	      locs ;; we don't return gt because loc is not part of the system
	      )
	  )
	nil
	)
    )
  )

(defun send-something1 (loc socket desc stream str)
  (declare (ignore loc))
  (declare (ignore stream))
  (if socket
      (let ((m (sb-bsd-sockets:socket-send socket str nil)))
	(declare (ignore m))
	;; TO COMMENT 2
	;;(print-eml-loc loc (format nil "--successfully sent ~A bytes through socket" m))
	nil
	)
      (let* ((n   (length str))
	     (buf (coerce str 'simple-base-string))
	     (m   (sb-posix:write desc (sb-sys:vector-sap buf) n)))
	(if (= n m)
	    (progn
	      ;; TO COMMENT 2
	      ;;(print-eml-loc loc (format nil "--successfully sent ~A bytes out of ~A through pipe" m n))
	      nil)
	    (error "did not send the correct number of bytes")
	    )
	)
      )
  )

(defun send-something2 (loc socket desc stream str)
  (declare (ignore loc))
  (declare (ignore socket))
  (declare (ignore desc))
  (write str :stream stream)
  (write-char #\linefeed stream)
  ;;(write "#\Newline" :stream stream)
  ;;(finish-output stream)
  ;;(write :eof :stream stream)
  (finish-output stream)
  ;;
  ;;(force-output stream)
  ;;(clear-output stream)
  ;;(print-eml-loc loc (format nil "sent: ~A" str))
  ;; TO COMMENT 2
  ;;(print-eml-loc loc (format nil "--successfully sent ~A bytes through socket" m))
  nil
  )

(defun send-something (loc socket desc stream str)
  (if new-send-receive
      (send-something2 loc socket desc stream str)
      (send-something1 loc socket desc stream str)
      )
  )

(defun send-port-number1 (loc port socket desc stream)
  (let ((str  (concatenate 'string (prin1-to-string port) "!")))
    (print-eml-loc loc (concatenate 'string "--sending port number " str))
    (send-something loc socket desc stream str)
    )
  )

(defun send-port-number2 (loc port socket desc stream)
  (print-eml-loc loc (format nil "--sending port number ~A" port))
  (send-something loc socket desc stream port)
  )

(defun send-port-number (loc port socket desc stream)
  (if new-send-receive
      (send-port-number2 loc port socket desc stream)
      (send-port-number1 loc port socket desc stream)
      )
  )

(defun send-public-key1 (loc socket desc stream)
  ;; !! WON'T WORK, pbkey is not a string
  (let ((str (concatenate 'string pbkey "!")))
    (print-eml-loc loc (concatenate 'string "--sending public key " str))
    (send-something loc socket desc stream str)
    )
  )

(defun send-public-key2 (loc socket desc stream)
  (print-eml-loc loc (format nil "--sending public key ~A" pbkey))
  (send-something loc socket desc stream pbkey)
  )

(defun send-public-key (loc socket desc stream)
  (if new-send-receive
      (send-public-key2 loc socket desc stream)
      (send-public-key1 loc socket desc stream)
      )
  )

(defun connect (loc socket host port)
  (handler-case
      (sb-bsd-sockets:socket-connect socket (sb-bsd-sockets:make-inet-address host) port)
    (sb-bsd-sockets:connection-refused-error
	(c)
      (declare (ignore c))
      (print-eml-loc loc "connection failed, will retry in 1 seconds")
      ;; sleep for 1s
      (sleep 1)
      (connect loc socket host port)))
  )

(defun socket-to-fd (socket)
  (sb-bsd-sockets:socket-file-descriptor socket)
  )

(defun socket-to-stream (socket)
  (sb-bsd-sockets:socket-make-stream socket :input t :output t :serve-events t);; :buffering :none)
  )

;; 'loc' connects to the list of locations 'gt', and make itself known
;; by sending its port number.  The locatios in 'gt' can find out who
;; the incoming connection request is by looking at the received port
;; number and checking it against the configuration file.
(defun connect-to (loc port gt)
  (print-eml-loc loc (format nil "~A machine(s) to connect to" (list-length gt)))
  (if gt
      (let* ((nfo    (car gt))
	     (h      (nfo-host nfo))
	     (p      (nfo-port nfo))
	     (socket (new-socket))
	     (desc   (socket-to-fd socket))
	     (stream (socket-to-stream socket)))
	(print-eml-loc loc (format nil "--connecting to ~A" (nfo-id nfo)))
	;; adds the socket to nfo
	(setf (nfo-socket nfo) socket)
	;; adds socket description to nfo
	(setf (nfo-desc nfo) desc)
	;; adds socket stream to nfo
	(setf (nfo-stream nfo) stream)
	;; connects to host using socket
	(connect loc socket h p)
	;; send port number to host using socket
	(send-port-number loc port socket desc stream)
	;; if 'auth' is true 'loc' also send public key
	(if auth
	    (send-public-key loc socket desc stream)
	    )
	;; returns the updated nfo
	(cons nfo (connect-to loc port (cdr gt)))
	)
      nil
      )
  )

(defun gen-receive1 (loc socket desc stream n)
  (declare (ignore loc))
  (declare (ignore stream))
  (if socket
      (let ((buf nil))
	(sb-bsd-sockets:socket-receive socket buf n)
	)
      (let* ((buf (make-array n :element-type '(unsigned-byte 8))) ;; 'standard-char
	     (m (sb-posix:read desc (sb-sys:vector-sap buf) n)))
	;;(print buf)
	;;(print m)
	(values (sb-ext:octets-to-string buf) m nil)
	)
      )
  )

(defun gen-receive2 (loc socket desc stream n)
  (declare (ignore socket))
  (declare (ignore desc))
  (declare (ignore n))
  (declare (ignore loc))
  ;;(declare (ignore loc))
  ;;(declare (ignore stream))
  ;;(print-eml-loc loc "listening")
  ;;(listen stream)
  ;;(print-eml-loc loc "reading")
  (let ((x (read stream)))
    ;;(print-eml-loc loc (format nil "read ~A" x))
    x
    )
  )

(defun gen-receive (loc socket desc stream n)
  (if new-send-receive
      (gen-receive2 loc socket desc stream n)
      (gen-receive1 loc socket desc stream n)
      )
  )


;; (defun old-gen-receive (loc socket desc n)
;;   (declare (ignore loc))
;;   (if socket
;;       (let ((buf nil))
;; 	(sb-bsd-sockets:socket-receive socket buf n)
;; 	)
;;       (let* ((buf (make-string n))
;; 	     (m (sb-posix:read desc (sb-sys:vector-sap buf) n)))
;; 	;;(print n)
;; 	;;(print m)
;; 	(values (coerce buf '(simple-array character (*))) m nil)
;; 	)
;;       )
;;   )

(defun receive-integer-aux (str loc socket desc stream)
  ;;(print-eml-loc loc (format nil "received so far: ~A" str))
  (multiple-value-bind (buf len peer)
      (gen-receive loc socket desc stream 1)
    (if (= len 0)
	(progn
	  (print-eml-loc loc (format nil "error: received empty vector from ~A" peer))
	  (print-eml-loc loc (format nil "received so far: ~A" str))
	  (error "received empty vector")
	  )
	(if (= len 1)
	    (if (string= buf "!")
		str
		(receive-integer-aux (concatenate 'string str buf) loc socket desc stream)
		)
	    (progn
	      (print-eml-loc loc (format nil "error: received vector of len > 1 (~A,~A)" len buf))
	      (error "received vector of len > 1")
	      )
	    )
	)
    )
  )

(defun receive-integer1 (loc socket desc stream)
  ;; TO COMMENT 2
  ;;(print-eml-loc loc "waiting to receive integer")
  (let* ((buf nil)
	 (str (receive-integer-aux buf loc socket desc stream))
	 (n (ignore-errors (parse-integer str))))
    (if n
	(progn
	  ;;(print-eml-loc loc str)
	  ;; TO COMMENT 2
	  ;;(print-eml-loc loc (format nil "--received integer ~A" n))
	  n
	  )
	(progn
	  (print-eml-loc loc (format nil "error: did no receive integer: ~A" str))
	  (error "did not receive integer")
	  )
	)
    )
  )

(defun receive-integer2 (loc socket desc stream)
  ;; TO COMMENT 2
  ;;(print-eml-loc loc "waiting to receive integer")
  (gen-receive loc socket desc stream 0)
  )

(defun receive-integer (loc socket desc stream)
  (if new-send-receive
      (receive-integer2 loc socket desc stream)
      (receive-integer1 loc socket desc stream)
      )
  )


(defun update-nfos-with-socket (port socket nfos)
  (if nfos
      (let ((nfo (car nfos)))
	(multiple-value-bind (updated rest)
	    (update-nfos-with-socket port socket (cdr nfos))
	  (if (= port (nfo-port nfo))
	      (progn
		(setf (nfo-socket nfo) socket)
		(setf (nfo-desc   nfo) (socket-to-fd socket))
		(setf (nfo-stream nfo) (socket-to-stream socket))
		(values (cons nfo updated) rest)
		)
	      (values updated (cons nfo rest))
	      )
	  )
	)
      (values nil nil)
      )
  )

(defun wait-for-connections (loc server connected lt)
  (print-eml-loc loc (format nil "waiting on ~A connection(s)" (list-length lt)))
  (if lt
      (multiple-value-bind (socket peer)
	  (sb-bsd-sockets:socket-accept server)
	(print-eml-loc loc (format nil "received connection request from ~A" peer))
	(let* ((fd   (socket-to-fd socket))
	       (strm (socket-to-stream socket))
	       (port (receive-integer loc socket fd strm)))
	  (multiple-value-bind (updated restlt)
	      (update-nfos-with-socket port socket lt)
	    (wait-for-connections loc server (append connected updated) restlt)
	    )
	  )
	)
      connected
      )
  )

(defun boot-up-conn (loc ip port conf)
  (print-eml-loc loc "connecting")
  (let* ((lt (get-lt-locations loc ip port conf))
	 (gt (get-gt-locations loc ip port conf))
	 (ltsize (list-length lt))
	 (gtsize (list-length gt)))
    (print-eml-loc loc (format nil "~A higher machine(s), ~A lower machine(s)" gtsize ltsize))
    (print-eml-loc loc "connecting to higher machines")
    (let ((lstgt (connect-to loc port gt)))
      (print-eml-loc loc "waiting on connections from lower machines")
      (let ((server (create-server-socket loc ip port)))
	(let ((lstlt (wait-for-connections loc server nil lt)))
	  (print-eml-loc loc "connected")
	  ;; forking to keep listening for inputs: keep-listening-for-inputs
	  (values server (append lstlt lstgt))
	  )
	)
      )
    )
  )

(defun boot-up-prog (loc conf spec)
  (print-eml-loc loc "loading program")
  (let ((parameters (conf-parameters conf)))
    ;;(print-eml-loc loc (format nil "parameters: ~A" parameters))
    ;;(load spec-file)
    (let ((program (reduce (lambda (prg param)
			     (funcall prg (mk-arg (parameter-value param))))
			   parameters
			   :initial-value (funcall spec))))
      (print-eml-loc loc "applied parameters")
      (funcall program (mk-arg loc))
      )
    )
  )

;; (defun receive-one-message-aux (loc str socket desc)
;;   (multiple-value-bind (buf len peer)
;;       (gen-receive loc socket desc 1)
;;     (declare (ignore peer))
;;     (if (string= buf "!")
;;     ;; TO COMMENT 2
;;     ;;(print-eml-loc loc (format nil "--received ~A bytes from ~A" len peer))
;;     (receive-one-message-len-aux loc
;; 				 (- n len)
;; 				 (concatenate 'string str buf)
;; 				 socket
;; 				 desc)
;;     )
;;   )

(defun receive-one-message-len-aux (loc n str socket desc stream)
  (if (= n 0)
      str
      (if (< n 0)
	  (error "receive-one-message-len-aux:negative-length")
	  (multiple-value-bind (buf len peer)
	      (gen-receive loc socket desc stream n)
	    (declare (ignore peer))
	    (let ((nbuf (subseq buf 0 len)))
	      ;; TO COMMENT 2
	      ;;(print-eml-loc loc (format nil "--received ~A bytes from ~A (~A)" len peer (length nbuf)))
	      ;;(if (not (string= str ""))
	      ;;  (print-eml-loc loc (format nil "tail(~A): ~A" (length nbuf) nbuf))
	      ;;)
	      (receive-one-message-len-aux loc
					   (- n len)
					   (concatenate 'string str nbuf)
					   socket
					   desc
					   stream)
	      )
	    )
	  )
      )
  )

;; receive-one-message works as follows:
;;  (1) first we receive the size of the message, say n
;;  (2) then we read the message itself, by reading n bytes
(defun receive-one-message-len (loc socket desc stream)
  (let ((n (receive-integer loc socket desc stream)))
    ;; TO COMMENT 2
    ;;(print-eml-loc loc (format nil "ready to receive ~A bytes" n))
    (let ((str (receive-one-message-len-aux loc n "" socket desc stream)))
      ;; TO COMMENT
      ;;(print-eml-loc loc (format nil "received: ~A" str))
      str
      )
    )
  )

(defun receive-one-message-len-parse1 (b loc socket desc stream)
  ;; we use read-from-string to deserialize/unmarshal a structure
  (if b
      (let ((str (receive-one-message-len loc socket desc stream)))
	(handler-case
	    ;;(read-from-string str) ;; Error on big strings?
	    (read (make-string-input-stream str))
	  (error (c)
	    ;;(print-eml-loc loc (format nil "error while reading string ~A" str))
	    (print-eml-loc loc (format nil "error while reading string"))
	    (print-eml-loc loc "propagating the error")
	    (error c)
	    )
	  )
	)
      ;;(ms:unmarshal (receive-one-message-len loc socket desc stream))
      (receive-one-message-len loc socket desc stream)
      )
  )

(defun receive-one-message-len-parse2 (b loc socket desc stream)
  (declare (ignore b))
  ;;(gen-receive loc socket desc stream 0)
  (ms:unmarshal (gen-receive loc socket desc stream 0))
  )

(defun receive-one-message-len-parse (b loc socket desc stream)
  (if new-send-receive
      (receive-one-message-len-parse2 b loc socket desc stream)
      (receive-one-message-len-parse1 b loc socket desc stream)
      )
  )

(defun send-message-aux (loc socket desc stream msg)
  ;; first we send the message
  ;;(send-something loc socket desc stream msg)
  (send-something loc socket desc stream (ms:marshal msg))
  ;; send we send the end char "!"
  ;;(send-something loc socket desc stream "!")
  )

;; This is the old send-message function that was using fork when
;; the sending has to be delayed.  The new function uses threads.
(defun send-message-xxx (loc socket desc stream msg delay)
  (if (and delay (> delay 0))
      (let ((n (sb-posix:fork)))
	(if (= n 0)
	    (progn ;; the child sends the message
	      (sleep (float (/ delay 1000)))
	      (send-message-aux loc socket desc stream msg)
	      ;;(sb-ext:exit)
	      (sb-ext:exit)
	      )
	    (print-eml-loc loc "child is going to send the delayed message")
	    )
	)
      (progn
	(send-message-aux loc socket desc stream msg)
	()
	)
      )
  )

(defun send-message (loc socket desc stream msg delay)
  (if (and delay (> delay 0))
      (progn
	(print-eml-loc loc (format nil "new thread is going to send the delayed message (~A ms)" delay))
	(sb-thread:make-thread
	 (lambda ()
	   (progn
	     (sleep (float (/ delay 1000)))
	     (send-message-aux loc socket desc stream msg)
	     ))
	 :name "sender")
	)
      (progn
	(send-message-aux loc socket desc stream msg)
	()
	)
      )
  )

(defun send-length-and-message-aux (loc socket desc stream slen str)
  ;; first we send the size of the message
  ;; TO COMMENT 2
  ;;(print-eml-loc loc (format nil "--sending message size ~A" slen))
  (send-something loc socket desc stream slen)
  ;; send we send the message itself
  ;; TO COMMENT 2
  ;;(print-eml-loc loc "--sending message")
  (send-something loc socket desc stream str)
  )

;; This is the old send-length-and-message function, the new one uses
;; threads instead of fork.
(defun send-length-and-message-xxx (loc socket desc stream str delay)
  (let* ((len  (length str))
	 (slen (concatenate 'string (prin1-to-string len) "!")))
    (if (and delay (> delay 0))
	(let ((n (sb-posix:fork)))
	  (if (= n 0)
	      (progn ;; the child sends the message
		(sleep (float (/ delay 1000)))
		(send-length-and-message-aux loc socket desc stream slen str)
		;;(sb-ext:exit)
		(sb-ext:exit)
		)
	      (print-eml-loc loc "child is going to send the delayed message")
	    )
	  )
	(progn
	  (send-length-and-message-aux loc socket desc stream slen str)
	  ()
	  )
	)
    )
  )

(defun mk-directed-message (delay loc msg)
  (make-pair :fst delay :snd (make-pair :fst loc :snd msg))
  )

(defun send-length-and-message-dmh (loc dest socket desc stream str delay)
  (if (and delay (> delay 0))
      ;; If delay > 0 and the delayed message handler is active then
      ;; we have to send a message to the handler instead of the destination.
      ;; (This is done by send-output-dmh)
      ;; The content of the message should be (delay,dest,msg) instead of just msg.
      (let* ((msg  (format nil "~S" (mk-directed-message delay dest str)))
	     (len  (length msg))
	     (slen (concatenate 'string (prin1-to-string len) "!")))
	(print-eml-loc loc "sending delayed message to delayed message handler")
	(send-length-and-message-aux loc socket desc stream slen msg)
	)
      (let* ((len  (length str))
	     (slen (concatenate 'string (prin1-to-string len) "!")))
	(send-length-and-message-aux loc socket desc stream slen str)
	()
	)
      )
  )

(defun send-length-and-message-threads (loc dest socket desc stream str delay)
  (declare (ignore dest))
  (let* ((len  (length str))
	 (slen (concatenate 'string (prin1-to-string len) "!")))
    (if (and delay (> delay 0))
	(progn
	  ;;(print-eml-loc loc (format nil "new thread is going to send the delayed message (~A ms)" delay))
	  (sb-thread:make-thread
	   (lambda ()
	     (progn
	       ;;(print-eml-loc loc "going to sleep")
	       (sleep (float (/ delay 1000)))
	       ;;(print-eml-loc loc "waking up")
	       ;;(print-eml-loc loc "sending message")
	       (send-length-and-message-aux loc socket desc stream slen str)
	       ;;(print-eml-loc loc "message sent")
	       ))
	   :name "sender")
	  ;;(print-eml-loc loc "thread is sending message")
	  ()
	  )
	(progn
	  (send-length-and-message-aux loc socket desc stream slen str)
	  ()
	  )
	)
    )
  )

(defun send-length-and-message (loc dest socket desc stream str delay)
  (handler-case
      (if dmh
	  (send-length-and-message-dmh     loc dest socket desc stream str delay)
	  (send-length-and-message-threads loc dest socket desc stream str delay)
	  )
    (error
	(c)
      (print-eml-loc loc (format nil "send-length-and-message error: ~A" c))
      (error c)
      )
    )
  )

(defun send-length-and-message-format1 (b loc dest socket desc stream info delay)
  ;; we use format to serialize/marshal
  (if b
      (send-length-and-message loc dest socket desc stream (format nil "~S" info) delay)
      ;;(send-length-and-message loc socket desc stream (ms:marshal info) delay)
      (send-length-and-message loc dest socket desc stream info delay)
      )
  )

(defun send-length-and-message-format2 (b loc dest socket desc stream info delay)
  (declare (ignore b))
  (declare (ignore dest))
  (send-message loc socket desc stream info delay)
  )

(defun send-length-and-message-format (b loc dest socket desc stream info delay)
;;  (print-eml-loc loc (format nil "sending message to ~A" dest))
;;  (if (string<= "client" dest)
;;      (send-length-and-message-format1 b loc socket desc stream info delay)
    (if (and new-send-receive (not dmh))
	(send-length-and-message-format2 b loc dest socket desc stream info delay)
      (send-length-and-message-format1 b loc dest socket desc stream info delay)
      )
;;    )
  )

(defun handle-new-connection (loc outfd stream socket)
  ;; TO COMMENT 2
  ;;(print-eml-loc loc "new connection handler waiting")
  ;; now we're going to receive messages from socket and
  ;; forward them to outfd
  (let* ((desc   (socket-to-fd socket))
	 (strm   (socket-to-stream socket))
	 (str    (receive-one-message-len-parse nil loc socket desc strm))
	 (b      nil)
	 (sock   nil)
	 (delay  nil))
    ;; TO COMMENT
    ;;(print-eml-loc loc (format nil "--received message ~A, forwarding it" str))
    ;; TO COMMENT 2
    ;;(print-eml-loc loc "--received message, forwarding it")
    ;; The destination is loc because we send a message to ourselves
    (send-length-and-message-format b loc loc sock outfd stream str delay)
    (handle-new-connection loc outfd stream socket)
    )
  )

(defun keep-listening-for-inputs (loc outfd stream server)
  ;; Listen for incoming connections
  (let ((socket (sb-bsd-sockets:socket-accept server))
	(n (sb-posix:fork)))
    (if (= n 0)
	(progn ;; new process
	  (print-eml-loc loc "handling new connection")
	  (handler-case (handle-new-connection loc outfd stream socket)
	    (error
		(c)
	      (print-eml-loc loc (format nil "handler died with error: ~A" c))
	      )
	    )
	  (print-eml-loc loc "handler stopped")
	  ;;(sb-ext:exit)
	  (sb-ext:exit)
	  )
	(keep-listening-for-inputs loc outfd stream server) ;; parent process
	;; (make-process "Connection handler"
	;; 		     #'handle-new-connection
	;; 		     loc
	;; 		     socket)
	)
    )
  )

(defun fd2stream (fd)
  (sb-sys:make-fd-stream fd :input t :output t :serve-events t);; :buffering :none)
  )

(defun mk-nfo-from-fd (id fd)
  (make-nfo :id     id
	    :host   nil
	    :port   nil
	    :name   nil
	    :member nil
	    :socket nil
	    :desc   fd
	    :stream (fd2stream fd))
  )

(defun add-fd-to-nfos (nfos id fd)
  (cons (mk-nfo-from-fd id fd) nfos)
  )

(defun boot-up (ident conf-file spec)
  (print-eml-loc ident "reading configuration file")
  (let* ((conf     (parse-conf-file conf-file))
	 (groups   (conf-groups conf))
	 (location (get-id-in-groups ident groups))
	 (loc      (location-id   location))
	 (ip       (location-host location))
	 (port     (location-port location))
	 (group    (find-group-id loc groups))
	 (member   (group-member group)))
    (if auth
	;; !! CHANGE THESE VALUES
	(progn (setq pbkey (ironclad:make-public-key :dsa :p 10 :q 10 :g 10 :y 10))
	       (setq prkey (ironclad:make-public-key :dsa :p 10 :q 10 :g 10 :y 10 :x 10))
	       )
	)
    (multiple-value-bind (server nfos)
	(boot-up-conn loc ip port conf)
      (multiple-value-bind (infd outfd)
	  (sb-posix:pipe)
	;;(print-eml-loc loc (format nil "created pipe, in: ~A, out: ~A" infd outfd))
	(let ((new-nfos (add-fd-to-nfos nfos nil infd))
	      (n (sb-posix:fork)))
	  ;; I was trying to use posix-fork instead of sb-posix:fork, but I'm not sure what it does!
	  (if (= n 0)
	      (progn ;; new process
		(print-eml-loc loc "forked listener (child), listening for inputs")
		(keep-listening-for-inputs (concatenate 'string loc "-h")
					   outfd
					   (fd2stream outfd)
					   server)
		)
	      (progn ;; parent process
		(print-eml-loc loc (format nil "forked listener (parent,~A), running process" n))
		(let ((program (if member (boot-up-prog loc conf spec) nil)))
		  (values loc member new-nfos program)
		  )
		)
	      )
	  )
	)
      )
    )
  )

;; (defun get-sockets-from-nfos (nfos)
;;   (mapcar (lambda (nfo) (nfo-socket nfo)) nfos)
;;   )

;; (defun receive-messages1 (loc nfos)
;;   (print-eml-loc loc "select...")
;;   (sb-alien:with-alien ((read-fds  (sb-alien:struct sb-unix:fd-set))
;;                         (write-fds (sb-alien:struct sb-unix:fd-set))
;;                         (err-fds   (sb-alien:struct sb-unix:fd-set)))
;;     (sb-unix:fd-zero read-fds)
;;     (sb-unix:fd-zero write-fds)
;;     (sb-unix:fd-zero err-fds)
;;     (let ((fds (mapcar (lambda (nfo) (socket-to-fd (nfo-socket nfo))) nfos))
;; 	  (num (list-length nfos)))
;;       (mapcar (lambda (fd) (sb-unix:fd-set fd read-fds)) fds)
;;       (multiple-value-bind (result readfds writefds errfds)
;; 	  (sb-unix:unix-select num (sb-alien:addr read-fds) write-fds err-fds nil nil)
;; 	(declare (ignore writefds))
;; 	(declare (ignore errfds))
;; 	(print-eml-loc loc (format nil "...left select with result ~A" result))
;; 	;;(sb-ext:make-fd-stream fd)
;; 	;;(sb-sys:add-fd-handler fd direction function)
;; 	(if readfds
;; 	    (reduce (lambda (msgs socket) (append msgs (receive-one-message loc socket)))
;; 		    readfds
;; 		    :initial-value nil)
;; 	    (error "receive-messages:select:nil")
;; 	    )
;; 	)
;;       )
;;     )
;;   )

;; (defun receive-messages2 (loc nfos)
;;   (if nfos
;;       (let* ((nfo    (car nfos))
;; 	     (socket (nfo-socket nfo))
;; 	     (rest   (cdr nfos))
;; 	     (fd     (socket-to-fd socket)))
;; 	(multiple-value-bind (result readfds writefds errfds)
;; 	    (sb-unix:unix-select 1 fd 0 0 0 0)
;; 	  (declare (ignore result))
;; 	  (declare (ignore writefds))
;; 	  (declare (ignore errfds))
;; 	  (if (= fd readfds)
;; 	      (receive-one-message loc socket)
;; 	      (receive-messages2 loc (append rest (cons nfo nil)))
;; 	      )
;; 	  )
;; 	)
;;       nil
;;       )
;;   )

(defun xxxx-add-nfo-handlers (loc nfos)
  (if nfos
      ;; we create the handlers
      (let* ((nfo    (car nfos))
	     (socket (nfo-socket nfo))
	     (fd     (nfo-desc   nfo))
	     (strm   (nfo-stream nfo)))
	;;(print-eml-loc loc (format nil "creating fd handler for ~A" fd))
	(sb-sys:with-fd-handler
	    (fd
	     :input
	     #'(lambda (x)
		 (declare (ignore x)) ;; x should be fd
		 (let ((msgs (receive-one-message-len-parse t loc socket fd strm)))
		   (setf nuprl-msgs
			 (append (if (listp msgs) msgs (cons msgs nil)) nuprl-msgs)))))
	  (xxxx-add-nfo-handlers loc (cdr nfos))
	  )
	)
      ;; we wait for an event to happen
      (sb-sys:serve-event)
      )
  )

(defun add-nfo-handlers (loc nfos)
  (declare (ignore loc))
  (declare (ignore nfos))
  ;; we wait for an event to happen
  (sb-sys:serve-event)
  ;; (handler-case (sb-sys:serve-event)
  ;;   (error (c)
  ;;     (print-eml-loc loc (format nil "an error occurred: ~A" c)))
  ;;   )
  )

(defun receive-messages (loc nfos)
  (setf nuprl-msgs nil)
  (add-nfo-handlers loc nfos)
  nuprl-msgs
  )

;; (defun receive-messages (loc nfos)
;;   (declare (ignore loc))
;;   (declare (ignore nfos))
;;   (sb-sys:serve-event)
;;   )

(defun get-delay-of-intransit-message (dmsg) (pair-fst dmsg))
(defun get-loc-of-intransit-message   (dmsg) (pair-fst (pair-snd dmsg)))
(defun get-msg-of-intransit-message   (dmsg) (pair-snd (pair-snd dmsg)))

(defun run-hdf (loc program msg)
  (declare (ignore loc))
  ;;(print-eml-loc loc (format nil "running program on ~A" msg))
  (if (inl-p program)
      (funcall (inl-val program) msg)
      (if (inr-p program)
	  (progn
	    "done"
	    nil)
	  (error "run-program:neither-inl-nor-inr")
	  )
      )
  )

(defun run-hdf-time (loc program msg)
  (print-eml-loc loc (format nil "universal time(1): ~A s" (get-universal-time)))
  (print-eml-loc loc (format nil "real time(1): ~A ms" (get-internal-real-time)))
  (print-eml-loc loc (format nil "run time(1): ~A ms" (get-internal-run-time)))
  (let ((ret (run-hdf loc program msg)))
    (print-eml-loc loc (format nil "universal time(2): ~A s" (get-universal-time)))
    (print-eml-loc loc (format nil "real time(2): ~A ms" (get-internal-real-time)))
    (print-eml-loc loc (format nil "run time(2): ~A ms" (get-internal-run-time)))
    ret)
  )

(defun nuprl-list-to-lisp (lst)
  (if (pair-p lst)
      (cons (pair-fst lst) (nuprl-list-to-lisp (pair-snd lst)))
      nil
      )
  )

(defun lisp-list-to-nuprl (lst)
  (if lst
      (make-pair :fst (car lst) :snd (lisp-list-to-nuprl (cdr lst)))
      (make-axiom)
      )
  )

(defun intransit-messages-to-list (msgs)
  (nuprl-list-to-lisp msgs)
  )

(defun find-nfo-with-id (loc id nfos)
  (declare (ignore loc))
  (find id nfos :test #'(lambda (id nfo) (string= id (nfo-id nfo))))
  )

(defun send-output-dmh (loc outnfo nfos output)
  (let ((delay (get-delay-of-intransit-message output))
	(to    (get-loc-of-intransit-message   output))
	(msg   (get-msg-of-intransit-message   output)))
    ;; TO COMMENT
    ;;(print-eml-loc loc (format nil "sending msg ~A to ~A" msg to))
    (if (and (string= loc to) (= delay 0))
	;; then we don't send the message through the socket, instead we're going
	;; to directly apply the program to the message.
	(cons msg nil)
	(let ((nfo (if (> delay 0) outnfo (find-nfo-with-id loc to nfos))))
	  (let ((socket (nfo-socket nfo))
		(desc   (nfo-desc   nfo))
		(stream (nfo-stream nfo))
		(dest   (nfo-id     nfo))) ;; to and dest should be the same
	    (send-length-and-message-format t loc dest socket desc stream msg delay)
	    nil
	    )
	  )
	)
    )
  )

(defun send-output-threads (loc outnfo nfos output)
  (let ((delay (get-delay-of-intransit-message output))
	(to    (get-loc-of-intransit-message   output))
	(msg   (get-msg-of-intransit-message   output)))
    ;; TO COMMENT
    ;;(print-eml-loc loc (format nil "sending msg ~A to ~A" msg to))
    (if (and (string= loc to) (= delay 0))
	;; then we don't send the message through the socket, instead we're going
	;; to directly apply the program to the message.
	(cons msg nil)
	(let ((nfo (if (string= loc to) outnfo (find-nfo-with-id loc to nfos))))
	  (let ((socket (nfo-socket nfo))
		(desc   (nfo-desc   nfo))
		(stream (nfo-stream nfo))
		(dest   (nfo-id     nfo))) ;; to and dest should be the same
	    (send-length-and-message-format t loc dest socket desc stream msg delay)
	    nil
	    )
	  )
	)
    )
  )

(defun send-output (loc outnfo nfos output)
  (if dmh
      (send-output-dmh loc outnfo nfos output)
      (send-output-threads loc outnfo nfos output)
      )
  )

(defun send-outputs (loc outnfo nfos outputs inmsgs)
  (if outputs
      (let ((msgs (send-output loc outnfo nfos (car outputs))))
	(send-outputs loc outnfo nfos (cdr outputs) (append inmsgs msgs))
	)
      (progn
	;; TO COMMENT 2
	;;(print-eml-loc loc "all outputs have been sent")
	inmsgs
	)
      )
  )

(defun run-on-messages (loc program msgs outnfo nfos)
  (if (and program msgs)
      (let ((msg  (car msgs))
	    (rest (cdr msgs)))
	(let* ((pair        (run-hdf loc program (mk-arg msg)))
	       (new-program (pair-fst pair))
	       (outputs     (intransit-messages-to-list (pair-snd pair))))
	  ;; TO COMMENT 2
	  ;;(print-eml-loc loc (format nil "sending ~A messages" (list-length outputs)))
	  ;; inmsgs are the messages from outputs that we have to send to ourselves
	  (let ((inmsgs (send-outputs loc outnfo nfos outputs nil)))
	    (run-on-messages loc new-program (append rest inmsgs) outnfo nfos)
	    )
	  )
	)
      program
      )
  )

(defun run-distributed-program-loop (loc member outnfo nfos program)
  ;;(print-eml-loc loc "waiting for a new message")
  (let* ((msgs (receive-messages loc nfos)))
    ;; TO COMMENT 2
    ;;(print-eml-loc loc (format nil "received ~A messages, running program" (list-length msgs)))
    ;;(print-eml-loc loc (format nil "connections: ~A" nfos))
    ;; TO COMMENT
    ;;(if member nil (print-eml-loc loc (format nil "~A" msgs)))
    (let ((new-program (run-on-messages loc program msgs outnfo nfos)))
      (run-distributed-program-loop loc member outnfo nfos new-program)
      )
    )
  )

;; (defun run-distributed-program-handlers (loc nfos program)
;;   (if nfos
;;       ;; we create the handlers
;;       (let* ((nfo    (car nfos))
;; 	     (socket (nfo-socket nfo))
;; 	     (fd     (nfo-desc nfo)))
;; 	;;(print-eml-loc loc (format nil "creating fd handler for ~A" fd))
;; 	(sb-sys:with-fd-handler
;; 	    (fd
;; 	     :input
;; 	     #'(lambda (fd)
;; 		 (receive-one-message loc socket fd)))
;; 	  (run-distributed-program-handlers loc (cdr nfos) program)
;; 	  )
;; 	)
;;       ;; we wait for an event to happen
;;       (run-distributed-program-loop loc nfos program)
;;       )
;;   )

(defun run-distributed-program-loop-dummy (loc nfos stream n)
  ;;(print-eml-loc loc "waiting for messages")
  (let* ((msgs (receive-messages loc nfos)))
    ;;(declare (ignore msgs))
    ;; TO COMMENT 2
    ;;(when (zerop (mod n 100))
    (print-eml-loc loc (format nil "received messages ~A" msgs))
    (print-eml-loc loc (format nil "universal time: ~A s" (get-universal-time)))
    (print-eml-loc loc (format nil "real time: ~A ms" (get-internal-real-time)))
    (print-eml-loc loc (format nil "run time: ~A ms" (get-internal-run-time)))
    ;;)
    (when stream
      (write msgs :stream stream)
      (write-char #\linefeed stream)
      (finish-output stream)
      )
    (run-distributed-program-loop-dummy loc nfos stream (+ n 1))
    )
  )

(defun xxxx-run-distributed-program (ident config spec)
  (multiple-value-bind (loc member nfos program)
      (boot-up ident config spec)
    (print-eml-loc loc "bootup finished")
    (multiple-value-bind (infd outfd)
	(sb-posix:pipe)
      (let ((new-nfos (add-fd-to-nfos nfos loc infd))
	    (outnfo (mk-nfo-from-fd loc outfd)))
	;;(setf *foo* new-nfos)
	(if program
	    (progn
	      (print-eml-loc loc "running process")
	      (run-distributed-program-loop loc member outnfo new-nfos program)
	      )
	    (progn
	      (print-eml-loc loc "no program to run")
	      (let ((stream (if eml-debug
				(open "output" :direction :output :if-exists :supersede)
				nil)))
		(run-distributed-program-loop-dummy loc new-nfos stream -2)
		)
	      )
	    )
	)
      )
    )
  )

(defun set-handlers (loc member outnfo nfos program allnfos)
  (if nfos
      ;; we create the handlers
      (let* ((nfo    (car nfos))
	     (peer   (nfo-id     nfo))
	     (socket (nfo-socket nfo))
	     (fd     (nfo-desc   nfo))
	     (strm   (nfo-stream nfo)))
	;;(print-eml-loc loc (format nil "creating fd handler for ~A" fd))
	(sb-sys:with-fd-handler
	    (fd
	     :input
	     #'(lambda (x)
		 (declare (ignore x)) ;; x should be fd
		 (handler-case
		     (let ((msgs (receive-one-message-len-parse t loc socket fd strm)))
		       (setf nuprl-msgs
			     (append (if (listp msgs) msgs (cons msgs nil))
				     nuprl-msgs))
		       )
		   (error (c)
		     (print-eml-loc loc (format nil "error happened while receiving message: ~A" c))
		     (print-eml-loc loc (format nil "cleaning handler for ~A" peer))
		     (sb-sys:invalidate-descriptor fd)
		     (print-eml-loc loc (format nil "handler cleaned for ~A" peer))
		     )
		   )
		 )
	     )
	  (set-handlers loc member outnfo (cdr nfos) program allnfos)
	  )
	)
      (if program
	  (progn
	    (print-eml-loc loc "running process")
	    (run-distributed-program-loop loc member outnfo allnfos program)
	    )
	  (progn
	    (print-eml-loc loc "no program to run")
	    (let ((stream (if eml-debug
			      (open "output" :direction :output :if-exists :supersede)
			      nil)))
	      (run-distributed-program-loop-dummy loc allnfos stream 0)
	      )
	    )
	  )
      )
  )

(defun delayed-message-handler-get-msgs (loc)
  (declare (ignore loc))
  ;; Is that serve-event going to clash with the other serve events
  ;; event though they're now 2 different processes because of the fork?
  (setf delayed-nuprl-msgs nil)
  (sb-sys:serve-event)
  ;;(print-eml-loc loc (format nil "delayed-message-handler served ~A" delayed-nuprl-msgs))
  delayed-nuprl-msgs
  )

(defun delayed-message-handler-send-output (loc outnfo nfos output)
  ;;(print-eml-loc loc "delayed-message-handler sending output")
  (sb-thread:make-thread
   (lambda ()
     (let ((delay (get-delay-of-intransit-message output))
	   (to    (get-loc-of-intransit-message   output))
	   (msg   (get-msg-of-intransit-message   output)))
       (sleep (float (/ delay 1000)))
       (let ((nfo (if (string= loc to) outnfo (find-nfo-with-id loc to nfos))))
	 (let ((socket (nfo-socket nfo))
	       (desc   (nfo-desc   nfo))
	       (stream (nfo-stream nfo))
	       (dest   (nfo-id     nfo))
	       (newdelay nil))
	   (send-length-and-message-format nil loc dest socket desc stream msg newdelay)
	   )
	 )))
   :name "delayed-mesasge-handler-sender")
  )

(defun delayed-message-handler-send-outputs (loc outnfo nfos outputs)
  (if outputs
      (progn
	(delayed-message-handler-send-output loc outnfo nfos (car outputs))
	(delayed-message-handler-send-outputs loc outnfo nfos (cdr outputs))
	)
      ()
      )
  )

(defun delayed-message-handler-loop (loc outnfo nfos)
  (let ((msgs (delayed-message-handler-get-msgs loc)))
    ;;(print-eml-loc loc (format nil "delayed-message-handler got messages ~A" msgs))
    (delayed-message-handler-send-outputs loc outnfo nfos msgs)
    (delayed-message-handler-loop loc outnfo nfos)
    )
  )

(defun delayed-message-handler (loc infd outnfo nfos)
  (let ((n (sb-posix:fork)))
    (if (= n 0)
	(sb-sys:with-fd-handler
	    (infd
	     :input
	     #'(lambda (x)
		 (declare (ignore x)) ;; x should be infd
		 ;;(print-eml-loc loc "delayed-message-handler handling")
		 (handler-case
		     (let ((msgs (receive-one-message-len-parse t loc nil infd (fd2stream infd))))
		       ;;(print-eml-loc loc "delayed-message-handler received")
		       (setf delayed-nuprl-msgs
			     (append (if (listp msgs) msgs (cons msgs nil))
				     delayed-nuprl-msgs))
		       )
		   (error (c)
		     (print-eml-loc loc (format nil "[delayed-message-handler]error happened while receiving message: ~A" c))
		     (print-eml-loc loc (format nil "[delayed-message-handler]dying"))
		     (sb-sys:invalidate-descriptor infd)
		     (print-eml-loc loc (format nil "[delayed-message-handler]died"))
		     )
		   )
		 )
	     )
	  (progn
	    (delayed-message-handler-loop loc outnfo nfos)
	    (print-eml-loc loc "delayed-message-handler stopped")
	    (sb-ext:exit)
	    )
	  )
	(print-eml-loc loc "forked a delayed message handler")
	)
    )
  )

(defun run-distributed-program-dmh (ident config spec)
  (multiple-value-bind (loc member nfos program)
      (boot-up ident config spec)
    ;; we create a pipe so that the process can send messages to itself.
    ;; we only send messages to ourselves when the messages are delayed.
    (multiple-value-bind (in-fd-handler out-fd-proc)
	(sb-posix:pipe)
      (multiple-value-bind (in-fd-proc out-fd-handler)
	  (sb-posix:pipe)
	(let ((new-nfos        (add-fd-to-nfos nfos loc in-fd-proc))
	      (out-nfo-proc    (mk-nfo-from-fd loc out-fd-proc))
	      (out-nfo-handler (mk-nfo-from-fd loc out-fd-handler)))
	  (delayed-message-handler loc in-fd-handler out-nfo-handler nfos)
	  (set-handlers loc member out-nfo-proc new-nfos program new-nfos)
	  )
	)
      )
    )
  )

(defun run-distributed-program-threads (ident config spec)
  (multiple-value-bind (loc member nfos program)
      (boot-up ident config spec)
    ;; we create a pipe so that the process can send messages to itself.
    ;; we only send messages to ourselves when the messages are delayed.
    (multiple-value-bind (in-fd-proc out-fd-proc)
	(sb-posix:pipe)
      (let ((new-nfos (add-fd-to-nfos nfos loc in-fd-proc))
	    (outnfo (mk-nfo-from-fd loc out-fd-proc)))
	(set-handlers loc member outnfo new-nfos program new-nfos)
	)
      )
    )
  )

(defun run-distributed-program (ident config spec)
  (if dmh
      (run-distributed-program-dmh ident config spec)
      (run-distributed-program-threads ident config spec)
      )
  )

(defun old-mk-msg (hdr typ val)
  (make-pair :fst hdr :snd (make-pair :fst typ :snd val))
  )

(defun mk-msg (hdr typ val)
  (declare (ignore typ))
  (make-pair :fst hdr :snd val)
  )

(defun mk-propose-msg (n)
  (let ((prp (make-pair :fst n :snd n))
	(hdr (make-pair :fst "propose" :snd (make-axiom)))
	(typ "product(int,,list(token))"))
    (mk-msg hdr typ prp)
    )
  )

(defun mk-propose-dmsg (ldr n)
  (let ((msg (mk-propose-msg n)))
    (mk-directed-message 0 ldr msg)
    )
  )

(defun mk-bcast-msg-val (n v)
  (let ((prp (make-pair :fst n
			:snd (make-pair :fst v
					:snd (make-axiom))))
	(hdr (make-pair :fst "bcast" :snd (make-axiom)))
	(typ ""))
    (mk-msg hdr typ prp)
    )
  )

(defun mk-bcast-msg (n)
  (mk-bcast-msg-val n (concatenate 'string "DUMMY###client1###ZDUMMY#client" (prin1-to-string n)))
  )

(defun mk-bcast-dmsg (loc n)
  (mk-directed-message 0 loc (mk-bcast-msg n))
  )

(defun mk-swap (n protocol)
  (let ((prp (make-pair :fst n
			:snd (make-pair :fst protocol
					:snd (make-axiom))))
	(hdr (make-pair :fst "swap" :snd (make-axiom)))
	(typ ""))
    (mk-msg hdr typ prp)
    )
  )

(defun mk-swap-paxos (n)
  (mk-swap n "paxos")
  )

(defun mk-swap-23 (n)
  (mk-swap n "2/3")
  )

(defun mk-swap-paxos-dmsg (loc n)
  (mk-directed-message 0 loc (mk-swap n "paxos"))
  )

(defun mk-swap-23-dmsg (loc n)
  (mk-directed-message 0 loc (mk-swap n "2/3"))
  )

(defun run-sender-loop (fmkmsg loc dest n m socket desc stream)
  (if (> n m)
      (print-eml-loc loc "done")
      (let* ((msg   (funcall fmkmsg n))
	     (delay nil))
	;; TO COMMENT 2
	(print-eml-loc loc (format nil "sending ~A" msg))
	(print-eml-loc loc (format nil "universal time: ~A s" (get-universal-time)))
	(print-eml-loc loc (format nil "real time: ~A ms" (get-internal-real-time)))
	(print-eml-loc loc (format nil "run time: ~A ms" (get-internal-run-time)))
	(send-length-and-message-format t loc dest socket desc stream msg delay)
	;; sleeps for 1ms
	(sleep 0.001)
	(run-sender-loop fmkmsg loc dest (+ n 1) m socket desc stream)
	)
      )
  )

(defun run-sender (fmkmsg loc id n m conf-file)
  ;; loc connects to id (a leader)
  (let* ((socket   (new-socket))
	 (desc     (socket-to-fd socket))
	 (stream   (socket-to-stream socket))
	 (conf     (parse-conf-file conf-file))
	 (location (get-id-in-groups id (conf-groups conf))))
    ;; first we connect to id
    (connect loc socket (location-host location) (location-port location))
    (print-eml-loc loc (format nil "connected to ~A" id))
    ;; then we send proposals from n to m
    (run-sender-loop fmkmsg loc id n m socket desc stream)
    )
  )



;;;;;; --------- tests ---------

;; (defvar pax-list "paxos-list.lisp")
;; (defvar pax-conf "conf_paxos.emlc")

;; (defvar aneris-slow "aneris_slowrep_opt4_2lisp2.lisp")
;;(defvar aneris-conf "conf_aneris_local.emlc")
(defvar aneris-conf "../conf/conf_aneris_batching.emlc")

;;(load "paxos-list2.fasl")
(if cbn
    (load "aneris-batching-control2.fasl")
    ;;(load "aneris_fastrep2_opt4_2lisp3.fasl")
  (load "aneris_fastrep_opt4_2lisp2.fasl")
  )

(defvar main-spec #'main)
;;(defvar conf-file pax-conf)
(defvar conf-file aneris-conf)

(defun test-boot-up-conn (loc ip port)
  (let ((conf (parse-conf-file conf-file)))
    (boot-up-conn loc ip port conf)
    )
  )

(defun test-boot-up-prog (loc)
  (let ((conf (parse-conf-file conf-file)))
    (boot-up-prog loc conf main-spec)
    )
  )

;; tests acceptors
(defun test-acc1 (conf-file) (run-distributed-program "acc1" conf-file main-spec))
(defun test-acc2 (conf-file) (run-distributed-program "acc2" conf-file main-spec))
(defun test-acc3 (conf-file) (run-distributed-program "acc3" conf-file main-spec))
;; tests 2/3 locations
(defun test-loc1 (conf-file) (run-distributed-program "loc1" conf-file main-spec))
(defun test-loc2 (conf-file) (run-distributed-program "loc2" conf-file main-spec))
(defun test-loc3 (conf-file) (run-distributed-program "loc3" conf-file main-spec))
(defun test-loc4 (conf-file) (run-distributed-program "loc4" conf-file main-spec))
;; tests leaders
(defun test-ldr1 (conf-file) (run-distributed-program "ldr1" conf-file main-spec))
(defun test-ldr2 (conf-file) (run-distributed-program "ldr2" conf-file main-spec))
;; tests replicas
(defun test-rep1 (conf-file) (run-distributed-program "rep1" conf-file main-spec))
(defun test-rep2 (conf-file) (run-distributed-program "rep2" conf-file main-spec))
;; tests clients
(defun test-client1 (conf-file) (run-distributed-program "client1" conf-file main-spec))
;; sender
(defun test-propose (n m conf-file) (run-sender #'mk-propose-msg "xxx" "ldr2" n m conf-file))
(defun test-bcast   (n m conf-file) (run-sender #'mk-bcast-msg   "xxx" "rep2" n m conf-file))
(defun test-swap    (n conf-file)   (run-sender #'mk-swap-paxos  "xxx" "rep2" n n conf-file))


;; tsts acceptors
(defun tst-acc1 () (run-distributed-program "acc1" conf-file main-spec))
(defun tst-acc2 () (run-distributed-program "acc2" conf-file main-spec))
(defun tst-acc3 () (run-distributed-program "acc3" conf-file main-spec))
;; tsts 2/3 locations
(defun tst-loc1 () (run-distributed-program "loc1" conf-file main-spec))
(defun tst-loc2 () (run-distributed-program "loc2" conf-file main-spec))
(defun tst-loc3 () (run-distributed-program "loc3" conf-file main-spec))
(defun tst-loc4 () (run-distributed-program "loc4" conf-file main-spec))
;; tsts leaders
(defun tst-ldr1 () (run-distributed-program "ldr1" conf-file main-spec))
(defun tst-ldr2 () (run-distributed-program "ldr2" conf-file main-spec))
;; tsts replicas
(defun tst-rep1 () (run-distributed-program "rep1" conf-file main-spec))
(defun tst-rep2 () (run-distributed-program "rep2" conf-file main-spec))
;; tsts clients
(defun tst-client1 () (run-distributed-program "client1" conf-file main-spec))
;; sender
(defun tst-propose (n m) (run-sender #'mk-propose-msg "xxx" "ldr2" n m conf-file))
(defun tst-bcast   (n m) (run-sender #'mk-bcast-msg   "xxx" "rep2" n m conf-file))
(defun tst-swap    (n)   (run-sender #'mk-swap-paxos  "xxx" "rep2" n n conf-file))
(defun tst-swap23  (n)   (run-sender #'mk-swap-23     "xxx" "rep2" n n conf-file))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;      FUNCTIONS TO RUN PROCESSES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun dest (pair)
  (if (pair-p pair)
      (let ((msgs (pair-snd pair)))
	(print (format nil "outputs: ~A" msgs))
	(pair-fst pair) ;; this is the new protocol
	)
      (error "dest:length")
      )
  )

;; (defun run-on-messages (program msgs)
;;   (if (= 0 (length msgs))
;;       program
;;       (let ((msg (car msgs))
;; 	    (rest (car (cdr msgs))))
;; 	(print "new message")
;; 	(let ((out (run program msg)))
;; 	  (print "ran program")
;; 	  (let ((p (dest out)))
;; 	    (print "program updated")
;; 	    (run-on-messages p rest)
;; 	    )
;; 	  )
;; 	)
;;       )
;;   )

(defun get-loc-prog-from-system (loc system)
  ;; a system is an association list string -> program
  (assoc loc system :test #'string=)
  )

(defun get-program-from-system (loc system)
  ;; a system is an association list string -> program
  (cdr (get-loc-prog-from-system loc system))
  )

(defun upd-program-from-system (loc program system)
  ;; a system is an association list string -> program
  (mapcar
   (lambda (x)
     (if (string= (car x) loc)
	 (rplacd (get-loc-prog-from-system loc system) program)
	 x))
   system)
  )

(defun local-run-on-messages (system msgs n lim)
  (if (and msgs (or (< lim 0) (< n lim)))
      (let ((dmsg (car msgs))
	    (rest (cdr msgs)))
	;;(print "list of intransit messages is not empty")
	;;(print "in transit message: ~A" dmsg)
	(let ((loc   (get-loc-of-intransit-message   dmsg))
	      (msg   (get-msg-of-intransit-message   dmsg))
	      (delay (get-delay-of-intransit-message dmsg)))
	  ;;(print (format nil "message to ~A: ~A" loc msg))
	  ;;(print (format nil "-> ~A" loc))
	  ;;(print (format nil "selection process for: ~A" loc))
	  ;;(print (format nil "system ~A" system))
	  (if (= delay 0)
	      (let ((prg (get-program-from-system loc system)))
		;;(print "running process on message")
		(if prg
		    (let ((pair (run-hdf "xxx" prg (mk-arg msg))))
		      ;;(print "process finished running, extracting updated process and outputs")
		      (let ((nprg  (pair-fst pair))
			    (nmsgs (intransit-messages-to-list (pair-snd pair))))
			;;(print (format nil "new messages: ~A" nmsgs))
			;;(print "keep on running")
			(local-run-on-messages (upd-program-from-system loc nprg system)
					       (append rest nmsgs)
					       (+ n 1)
					       lim
					       )
			)
		      )
		    (progn
		      ;;(print (format nil "no program for ~A" loc))
		      (local-run-on-messages system rest (+ n 1) lim)
		      )
		    )
		)
	      (local-run-on-messages system rest (+ n 1) lim)
	      )
	  )
	)
      (progn
	;;(print "no more intransit messages")
	system)
      )
  )

(defun local-run-on-messages-seq (system msgs lim)
  (if msgs
      (let ((msg  (car msgs))
	    (rest (cdr msgs)))
	(local-run-on-messages-seq (local-run-on-messages system (cons msg nil) 0 lim) rest lim)
	)
      system
      )
  )

(defun local-run-on-messages-int-loop (system loc n lim)
  (if (< n lim)
      (let ((dmsg (mk-bcast-dmsg loc n)))
	(print (format nil "-> ~A" n))
	(local-run-on-messages-int-loop (local-run-on-messages system
							       (cons dmsg nil)
							       0
							       -1)
					loc
					(+ n 1)
					lim
					)
	)
      system
      )
  )

(defun local-run-on-messages-int (system loc n)
  (print "swap")
  (let ((system2 (local-run-on-messages system
					(cons (mk-swap-paxos-dmsg loc 1) nil)
					0
					-1)))
    (print "bcasts")
    (local-run-on-messages-int-loop system2 loc 2 n)
    )
  )

;; (defun test ()
;;   (let* ((program (funcall (funcall (mainTmp2) 2) "loc"))
;; 	 (msg1  (mk-msg (cons "foo" (cons nil nil)) "int" 3))
;; 	 (msg2  (mk-msg (cons "foo" (cons nil nil)) "int" 5))
;; 	 (msg3  (mk-msg (cons "foo" (cons nil nil)) "int" 7))
;; 	 (msgs  (cons msg1 (cons msg2 (cons msg3 nil)))))
;;     (declare (ignore msgs))
;;     (time (dest (run (dest (run (dest (run program msg1)) msg2)) msg3)))
;;     )
;;   )

(defun test (n)
  (let* ((conf       (parse-conf-file conf-file))
	 (spec       main-spec)
	 (parameters (conf-parameters conf))
	 (program0   (reduce (lambda (prg param) (mk-app prg (parameter-value param)))
			     parameters
			     :initial-value (funcall spec)))
	 (ldr1       (mk-app program0 "ldr1"))
	 (ldr2       (mk-app program0 "ldr2"))
	 (acc1       (mk-app program0 "acc1"))
	 (acc2       (mk-app program0 "acc2"))
	 (acc3       (mk-app program0 "acc3"))
	 (loc1       (mk-app program0 "loc1"))
	 (loc2       (mk-app program0 "loc2"))
	 (loc3       (mk-app program0 "loc3"))
	 (loc4       (mk-app program0 "loc4"))
	 (rep1       (mk-app program0 "rep1"))
	 (rep2       (mk-app program0 "rep2"))
	 (system     (acons "ldr1" ldr1
			    (acons "ldr2" ldr2
				   (acons "acc1" acc1
					  (acons "acc2" acc2
						 (acons "acc3" acc3
							(acons "loc1" loc1
							       (acons "loc2" loc2
								      (acons "loc3" loc3
									     (acons "loc4" loc4
										    (acons "rep1" rep1
											   (acons "rep2" rep2
												  nil)))))))))))))
    ;;(declare (ignore system))
    ;;(time agent)
    ;;(time (dest (run ldr2 msg1)))
    ;;(time (run-on-messages system (cons dmsg1 nil) 0 (- 0 1)))
    ;;(time (run-on-messages-seq system msgs n))
    (time (local-run-on-messages-int system "rep2" n))
    )
  )

;; (time (run (funcall (funcall (mainTmp2) 2) "loc") '('("foo") '("int" 1))))

;; (1) prepend program to what's above in a file test.lisp
;; (2) start sbcl in a terminal
;; (3) type (load "test.lisp")
;; (4) type (sb-ext:save-lisp-and-die "test.core" :executable t :toplevel 'test :purify t)
;; (5) test.core is now an executable that runs test as above
