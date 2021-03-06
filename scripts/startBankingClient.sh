# $1 configuration file
# $2 "read-only" for read-only transactions or "update" for update transactions
# $3 minimum number of clients
# $4 maximum number of clients
# $5 client count increment
# $6 aneris mode (INTERPRETED/LISP)

JARS=../jars/ShadowDB.jar
LOG_PROP=../conf/logging.properties
 
java -cp ${JARS} -Djava.util.logging.config.file=${LOG_PROP} edu.Cornell.Diversity.Test.BankingApp $1 $2 $3 $4 $5 $6
