# $1 number of clients
# $2 number of messages to broadcast per client
# $3 protocol type (PAXOS/TWOTHIRD)

JARS=../jars/ShadowDB.jar
CONF_FILE=../conf/conf_aneris.emlc
LOG_PROP=../conf/logging.properties
 
java -cp ${JARS} -Djava.util.logging.config.file=${LOG_PROP} edu.Cornell.Diversity.Test.TobcastApp ${CONF_FILE} $1 $2 $3
