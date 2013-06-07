# $1 configuration file
# $2 number of clients
# $3 number of messages to broadcast per client
# $4 Aneris's type (INTERPRETED/LISP)
# $5 protocol type (PAXOS/TWOTHIRD)

JARS=../jars/ShadowDB.jar
LOG_PROP=../conf/logging.properties
 
java -cp ${JARS} -Djava.util.logging.config.file=${LOG_PROP} edu.Cornell.Diversity.Test.TobcastApp $1 $2 $3 $4 $5
