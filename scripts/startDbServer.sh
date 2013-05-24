# $1 database number
# $2 "replicated" or "standalone"

#MIN_SIZE=1024
MIN_SIZE=512
#MAX_SIZE=3072
MAX_SIZE=1024
JARS=../jars/h2-1.3.170.jar:../jars/hsqldb.jar:../jars/derby.jar:../jars/ShadowDB.jar
CONF_FILE=../conf/conf_aneris.emlc
LOG_PROP=../conf/logging.properties

java -cp ${JARS} -Djava.util.logging.config.file=${LOG_PROP} -Xms${MIN_SIZE}m -Xmx${MAX_SIZE}m edu.Cornell.Diversity.ShadowDB.ShadowDBServer database$1 ${CONF_FILE} $2
