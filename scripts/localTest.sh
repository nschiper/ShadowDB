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

function tobcast2(){
    # Compile Lisp
    ./compile-aneris-lisp.sh

    # Replicas
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}rep1 --hide-menubar --geometry 100x20+0+0      -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep1; ./startAneris1-rep1-lisp.sh ${CONF1C}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep1"
    sleep 1
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}rep2 --hide-menubar --geometry 100x20+0+0      -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep2; ./startAneris2-rep2-lisp.sh ${CONF1C}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}rep2"
    sleep 1

    # Leaders
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}ldr1 --hide-menubar --geometry 100x20+0+0      -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr1; ./startAneris1-ldr1-lisp.sh ${CONF1C}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr1"
    sleep 1
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}ldr2 --hide-menubar --geometry 100x20+0+0      -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr2; ./startAneris2-ldr2-lisp.sh ${CONF1C}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}ldr2"
    sleep 1

    # Acceptors
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc1 --hide-menubar --geometry 100x20+0+0      -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc1; ./startAneris1-acc1-lisp.sh ${CONF1C}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc1"
    sleep 1
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc2 --hide-menubar --geometry 100x20+0+0      -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc2; ./startAneris2-acc2-lisp.sh ${CONF1C}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc2"
    sleep 1
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}acc3 --hide-menubar --geometry 100x20+0+0      -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc3; ./startAneris3-acc3-lisp.sh ${CONF1C}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}acc3"
    sleep 1

    # 2/3
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc1 --hide-menubar --geometry 100x20+0+0      -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc1; ./startAneris1-loc1-lisp.sh ${CONF1C}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc1"
    sleep 1
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc2 --hide-menubar --geometry 100x20+0+0      -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc2; ./startAneris2-loc2-lisp.sh ${CONF1C}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc2"
    sleep 1
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc3 --hide-menubar --geometry 100x20+0+0      -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc3; ./startAneris3-loc3-lisp.sh ${CONF1C}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc3"
    sleep 1
    ${TERMINAL} --working-directory=${DIR} --title=${EMLWINTITLE}loc4 --hide-menubar --geometry 100x20+0+0      -x bash -c "wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc4; ./startAneris4-loc4-lisp.sh ${CONF1C}; bash; wmctrl -r :ACTIVE: -N ${EMLWINTITLE}loc4"

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
    tobcast2)
	tobcast2
	;;
    *)
	echo "The only options are twothirds and stop"
	;;
esac
