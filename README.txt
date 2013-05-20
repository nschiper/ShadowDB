INSTALLATION
------------

1) Compile ShadowDB using Apache Ant by simply typing 'ant' in the ShadowDB subdirectory. This will create the file 'ShadowDB.jar' in the project root directory.
2) Compile Aneris: TODO 

CONFIGURATION
-------------

Configuring ShadowDB is done by editing the file 'conf_aneris.emlc'. Note that database identifiers must start with "database" and the first database must have identifier "database1". The database deployed at each replica can be chosen between 'h2', 'hsqldb', and 'derby' and is specified in the configuration file.

TESTING
-------

ShadowDB can be tested with the provided scripts 'startAneris.sh', 'startDbServer.sh', and 'startBankingClient.sh'. Each of one of these scripts takes parameters whose semantics are defined in the respective script file. Additionally, Aneris can be tested with the script 'startTobcastClient.sh'.
