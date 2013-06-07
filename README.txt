INSTALLATION
------------

1) Compile ShadowDB using Apache Ant by simply typing 'ant' in the
ShadowDB subdirectory. This will create the file 'ShadowDB.jar' in the
project root directory.

2) To compile Aneris, type 'make aneris'.  This requires mlton,
mlyacc, and mllex.  MLton can be downloaded from:
  http://sourceforge.net/projects/mlton/files/mlton/20100608/
I did not have any luck installing the FreeBSD package using pkg_add,
but extracting the archive was enough.  The archive already contains
these three binaries.

3) To generate the Nuprl library file, type 'make lib'.

NOTE: 1), 2), and 3) can be done by simply typing 'make'.

4) To install the SBCL Lisp compiler on FreeBSD, type 'pkg_add -r sbcl'.
We're using several packages that we've installed using quicklisp.
Here is how to get quicklisp and install these packages using quicklisp:
    o Download quicklisp.lisp here: http://www.quicklisp.org/beta/
    o run in sbcl: (load "quicklisp.lisp")
    o then: (quicklisp-quickstart:install)
    o then: (ql:quickload "cl-lex")
    o then: (ql:quickload "yacc")
    o then: (ql:quickload "marshal")
    o then: (ql:add-to-init-file)

   The Lisp version of Aneris can be compiled by typing 'make aneris-lisp'.


CONFIGURATION
-------------

Configuring ShadowDB is done by editing the file
'conf/conf_aneris.emlc'.  Note that database identifiers must start
with "database" and the first database must have identifier
"database1".  The database deployed at each replica can be chosen
between 'h2', 'hsqldb', and 'derby' and is specified in the
configuration file. Additionally, when using ShadowDB with Aneris the configuration file should
contain exactly one client per database (see sections %locations external clients
and %databases in the configuration file). 


TESTING
-------

ShadowDB can be tested with the provided scripts 'startAnerisX.sh'
(where X is between 1 and 4), 'startDbServer.sh', and
'startBankingClient.sh'.  Each of one of these scripts takes
parameters whose semantics are defined in the respective script file.
Additionally, Aneris can be tested with the script
'startTobcastClient.sh'. To start Aneris in Lisp mode, use the
scripts 'startAnerisX-lisp.sh'.


EXAMPLE
-------

This section explains how to test ShadowDB and Aneris in interpreted mode locally.  We
will use the conf/conf_aneris_local.emlc configuration file instead of
the conf/conf_aneris.emlc file.

1) First we start various Aneris processes.  They are supposed to run
on 4 machines, so we have 4 scripts to start these processes.  Go to
the scripts directory and type:

  ./startAneris1.sh local
  ./startAneris2.sh local
  ./startAneris3.sh local
  ./startAneris4.sh local

As mentioned above, these 4 scripts use conf_aneris_local.emlc instead
of conf_aneris.emlc.

Alternatively, you can type:

  make start1
  make start2
  make start3
  make start4

from the root directory.

2) Then we start ShadowDB by typing:

  make testrep1
  make testrep2

3) To test the database, you can run the startBankingClient script as
follows:

  make testbank
