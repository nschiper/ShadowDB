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
    pb)
	EVENTML_CMD="${EVENTML} --i ${PROG_OPT} --conf ${CONF_PB} --nuprl-defs ${ALLDEFS} --gc --extra "newsock:newprog" --id"
	;;
    tobcast)
	case $3 in
	    opt)
		echo "starting opt version of aneris"
		EVENTML_CMD="${EVENTML} --i ${PROG_OPT2} --conf ${CONF_TB} --nuprl-defs ${ALLDEFS} --gc --extra "newsock:newprog:silent" --id"
		;;
	    *)
		EVENTML_CMD="${EVENTML} --i ${PROG_NOOPT} --conf ${CONF_TB} --nuprl-defs ${ALLDEFS} --gc --extra "newsock:newprog:silent" --id"
		;;
	esac
	;;
    *)
	EVENTML_CMD="${EVENTML} --i ${PROG_OPT} --conf ${CONF_SMR} --nuprl-defs ${ALLDEFS} --gc --extra "newsock:newprog:silent" --id"
	;;
esac

TIMESTAMP=`date +%d.%m.%y-%H:%M:%S`
LOG_DIR="logs"
FI_CMD=$ANERIS"/InjectFaults"

# 2/3 consensus
if [ "$2" = "fi" ]; then
    ${FI_CMD} $3 ${EVENTML_CMD} loc4 &
else
    ${EVENTML_CMD} loc4 &
fi

echo "Started 2/3-consensus"
