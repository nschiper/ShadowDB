#!/bin/bash

S=scala
DIR=`pwd`
TERMINAL=gnome-terminal
EMLWINTITLE=runeml
CONF1C=../conf/conf_aneris_local_1client.emlc

function bank(){
    # Compile Lisp
    ./compile-aneris-lisp.sh

    # Lisp
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}m1 --hide-menubar --geometry 100x20+0+0      -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}m1; ./startAneris1-lisp.sh; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}m1"
    sleep 2
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}m2 --hide-menubar --geometry 100x20+350+0    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}m2; ./startAneris2-lisp.sh; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}m2"
    sleep 2
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}m3 --hide-menubar --geometry 100x20+700+0    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}m3; ./startAneris3-lisp.sh; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}m3"
    sleep 2
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}m4 --hide-menubar --geometry 100x20+1050+0   -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}m4; ./startAneris4-lisp.sh; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}m4"
    sleep 10

    # Clients
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}cl1 --hide-menubar --geometry 100x20+0+340    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}cl1; cd ..; make testrep1 ANERISMODE=LISP; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}cl1"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}cl2 --hide-menubar --geometry 100x20+350+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}cl2; cd ..; make testrep2 ANERISMODE=LISP; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}cl2"
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}cl3 --hide-menubar --geometry 100x20+700+340  -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}cl3; cd ..; make testrep3 ANERISMODE=LISP; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}cl3"

    # Bank
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}bk --hide-menubar --geometry 100x20+0+800    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}bk; cd ..; make testbank ANERISMODE=LISP; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}bk"
}

function tobcast(){
    # Compile Lisp
    ./compile-aneris-lisp.sh

    # Lisp
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}m1 --hide-menubar --geometry 100x20+0+0      -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}m1; ./startAneris1-lisp.sh ${CONF1C}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}m1"
    sleep 2
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}m2 --hide-menubar --geometry 100x20+350+0    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}m2; ./startAneris2-lisp.sh ${CONF1C}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}m2"
    sleep 2
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}m3 --hide-menubar --geometry 100x20+700+0    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}m3; ./startAneris3-lisp.sh ${CONF1C}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}m3"
    sleep 2
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}m4 --hide-menubar --geometry 100x20+1050+0   -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}m4; ./startAneris4-lisp.sh ${CONF1C}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}m4"
    sleep 10

    # tobcast
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}bk --hide-menubar --geometry 100x20+0+800    -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}bk; ./startTobcastClient.sh ${CONF1C} 1 2000 LISP PAXOS; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}bk"
}

case $1 in
    bank)
	bank
	;;
    tobcast)
	tobcast
	;;
    *)
	echo "The only options are twothirds and stop"
	;;
esac
