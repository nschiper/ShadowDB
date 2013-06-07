# $1 configuration file
# $2 "read-only" for read-only transactions or "update" for update transactions
# $3 number of bank accounts
# $4 minimum number of clients
# $5 maximum number of clients
# $6 client count increment

JARS=../jars/ShadowDB.jar
LOG_PROP=../conf/logging.properties
 
java -cp ${JARS} -Djava.util.logging.config.file=${LOG_PROP} edu.Cornell.Diversity.Test.BankingApp $1 $2 $3 $4 $5 $6
