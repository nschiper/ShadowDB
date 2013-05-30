INSTALLATION
------------

1) Compile ShadowDB using Apache Ant by simply typing 'ant' in the
ShadowDB subdirectory. This will create the file 'ShadowDB.jar' in the
project root directory.

2) To compile Aneris, type 'make aneris'.  This requires mlton,
mlyacc, and mllex.  MLton can be downloaded from
  http://sourceforge.net/projects/mlton/files/mlton/20100608/
I did not have any luck installing the FreeBSD package using pkg_add,
but extracting the archive was enough.  The archive already contains
these three binaries.

3) To generate the Nuprl library file, type 'make lib'.

NOTE: 1), 2), and 3) can be done by simply typing 'make'.

4) To install the SBCL Lisp compiler on FreeBSD, type 'pkg_add -r sbcl'.

CONFIGURATION
-------------

Configuring ShadowDB is done by editing the file 'conf_aneris.emlc'. Note that database identifiers must start with "database" and the first database must have identifier "database1". The database deployed at each replica can be chosen between 'h2', 'hsqldb', and 'derby' and is specified in the configuration file.

TESTING
-------

ShadowDB can be tested with the provided scripts 'startAneris.sh', 'startDbServer.sh', and 'startBankingClient.sh'. Each of one of these scripts takes parameters whose semantics are defined in the respective script file. Additionally, Aneris can be tested with the script 'startTobcastClient.sh'.
