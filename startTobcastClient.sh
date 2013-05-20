# $1 number of clients
# $2 number of messages to broadcast per client
# $3 protocol type (PAXOS/TWOTHIRD)

java -cp ShadowDB.jar -Djava.util.logging.config.file=logging.properties edu.Cornell.Diversity.Test.TobcastApp conf_aneris.emlc $1 $2 $3
