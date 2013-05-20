# $1 database number
# $2 "replicated" or "standalone"

java -cp h2-1.3.170.jar:hsqldb.jar:derby.jar:ShadowDB.jar -Djava.util.logging.config.file=logging.properties -Xms1024m -Xmx3072m edu.Cornell.Diversity.ShadowDB.ShadowDBServer database$1 conf_aneris.emlc $2
