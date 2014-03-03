# $1 database number
# $2 configuration file (use ../conf/conf_aneris_local.emlc to do a test on your local machine)
# $3 "replicated" or "standalone"
# $4 "INTERPRETED" or "LISP" whether it is the interpreted or Lisp version of Aneris that is running

MIN_SIZE=512
MAX_SIZE=1024
JARS=../jars/h2-1.3.170.jar:../jars/hsqldb.jar:../jars/derby.jar:../jars/ShadowDB.jar
LOG_PROP=../conf/logging.properties

if [  "$4" = "INTERPRETED" ]
then
   java -cp ${JARS} -Djava.util.logging.config.file=${LOG_PROP} -Xms${MIN_SIZE}m -Xmx${MAX_SIZE}m edu.Cornell.Diversity.ShadowDB.ShadowDBServer database$1 $2 $3 $4
else
   java -cp ${JARS} -Djava.util.logging.config.file=${LOG_PROP} -Xms${MIN_SIZE}m -Xmx${MAX_SIZE}m edu.Cornell.Diversity.ShadowDB.ShadowDBServerSMR database$1 $2
fi
