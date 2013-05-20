# $1 "read-only" for read-only transactions or "update" for update transactions
# $2 number of bank accounts
# $3 minimum number of clients
# $4 maximum number of clients
# $5 client count increment
 
java -cp ShadowDB.jar -Djava.util.logging.config.file=logging.properties edu.Cornell.Diversity.Test.BankingApp conf_aneris.emlc $1 $2 $3 $4 $5
