INSTALLATION
------------

1) Compile ShadowDB using Apache Ant by simply typing 'ant' in the ShadowDB subdirectory. This will create the file 'ShadowDB.jar' in the 'jars' directory.
2) Compile Aneris: type 'make aneris'.
3) Generate library file: type 'make lib'

to generate everything type: 'make'

CONFIGURATION
-------------

Configuring ShadowDB is done by editing the file 'conf/conf_aneris.emlc'. Note that database identifiers must start with "database" and the first database must have identifier "database1". The database deployed at each replica can be chosen between 'h2', 'hsqldb', and 'derby' and is specified in the configuration file.

TESTING
-------

ShadowDB can be tested with the scripts provided in the 'scripts' folder. Each of one of these scripts takes parameters whose semantics are defined in the respective script file.
