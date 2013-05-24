# $1 "read-only" for read-only transactions or "update" for update transactions
# $2 number of bank accounts
# $3 minimum number of clients
# $4 maximum number of clients
# $5 client count increment

JARS=../jars/ShadowDB.jar
CONF_FILE=../conf/conf_aneris.emlc
LOG_PROP=../conf/logging.properties
 
java -cp ${JARS} -Djava.util.logging.config.file=${LOG_PROP} edu.Cornell.Diversity.Test.BankingApp ${CONF_FILE} $1 $2 $3 $4 $5
