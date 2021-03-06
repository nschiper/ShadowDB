ANERIS=..

## EventML binary
EVENTML=${ANERIS}/bin/eventml

## configuration files
CONF_DF=${ANERIS}/conf/conf_aneris.emlc
CONF_LOC=${ANERIS}/conf/conf_aneris_local.emlc

## program files
PROG_OPT=${ANERIS}/examples/aneris_fastrep_opt4.prog
PROG_NOOPT=${ANERIS}/examples/aneris_fastrep_noopt.prog
PROG_OPT2=${ANERIS}/examples/aneris_fastrep2_opt4.prog

## library files
ALLDEFS=${ANERIS}/lib/alldefs

case $1 in
    local)
	EVENTML_CMD="${EVENTML} --i ${PROG_OPT} --conf ${CONF_LOC} --nuprl-defs ${ALLDEFS} --gc --extra "newsock:newprog" --id"
	;;
    *)
	EVENTML_CMD="${EVENTML} --i ${PROG_OPT} --conf ${CONF_DF} --nuprl-defs ${ALLDEFS} --gc --extra "newsock:newprog:silent" --id"
	;;
esac

FI_CMD=$ANERIS"/InjectFaults"
TIMESTAMP=`date +%d.%m.%y-%H:%M:%S`
LOG_DIR="logs"

# 2/3 consensus
if [ "$2" = "fi" ]; then
    ${FI_CMD} $3 ${EVENTML_CMD} loc2 &
else
    ${EVENTML_CMD} loc2 &
fi

sleep 10
echo "Started 2/3-consensus"

# Paxos - acceptors
  ${EVENTML_CMD} acc2 &

sleep 10

# Paxos - leaders
   ${EVENTML_CMD} ldr2 &

echo "Started Paxos"
sleep 10

# Replicas (The Aneris interface)
   ${EVENTML_CMD} rep2 &

echo "Started Aneris"
