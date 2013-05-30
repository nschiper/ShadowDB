# Copyright 2011 Cornell University
# Copyright 2012 Cornell University
# Copyright 2013 Cornell University
#
#
# This file is part of EventML - a tool aiming at specifying
# distributed protocols in an ML like language.  It is an interface
# to the logic of events and is compiled into Nuprl.  It is written
# by the NUPRL group of Cornell University, Ithaca, NY.
#
# EventML is a free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# EventML is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with EventML.  If not, see <http://www.gnu.org/licenses/>.
#
#  o Authors:     Vincent Rahli
#  o Affiliation: Cornell University, NUPRL group
#  o Date:        20 May 2011
#  o File name:   Makefile
#  o Description: .
#


SHELL=/bin/bash
PARSE_FILE=EML
NUPRL_ASCII_FILE=NuprlAscii
CONFIG_PARSE_FILE=Config
EVENTML_BIN_DIR=../bin
EVENTML_LIB_DIR=../lib
LOCAL_BIN_DIR=bin
DATE1=`date +%Y-%m-%d`
DATE2=`date`
TIME=`date +%T`
HOST=`hostname`
TMPDIR=/tmp
PARSE=parse
EXPORT=export
TERMS=terms
EVALUATORS=evaluators
BIN_NAME=eventml
PROFKIND=time
ARCH=x86
MLTON=mlton
MLLEX=mllex
MLYACC=mlyacc

VERSION=0.4

default:
	# you can:
	#   - generate a mlton binary: bin
	#   - clean the directory:     clean

clean-parse :
	rm -f ${PARSE}/*.grm.sml  ${PARSE}/*.grm.desc  ${PARSE}/*.grm.sig  ${PARSE}/*.lex.sml
	rm -f ${TERMS}/*.grm.sml  ${TERMS}/*.grm.desc  ${TERMS}/*.grm.sig  ${TERMS}/*.lex.sml
	rm -f ${EXPORT}/*.grm.sml ${EXPORT}/*.grm.desc ${EXPORT}/*.grm.sig ${EXPORT}/*.lex.sml

clean: clean-parse
	rm -Rf .cm mlton-control.sml

# ultra clean
uclean: clean
	rm -Rf alldefs*
	rm -Rf *~
	rm -Rf test*
	rm -Rf tmp*
	rm -Rf Paxos_*
	rm -Rf mlmon.out
	rm -Rf ${BIN_NAME}

mlton-base:
	${MLLEX}  ${PARSE}/${PARSE_FILE}.lex
	${MLYACC} ${PARSE}/${PARSE_FILE}.grm
	${MLLEX}  ${TERMS}/${NUPRL_ASCII_FILE}.lex
	${MLYACC} ${TERMS}/${NUPRL_ASCII_FILE}.grm
	${MLLEX}  ${EXPORT}/${CONFIG_PARSE_FILE}.lex
	${MLYACC} ${EXPORT}/${CONFIG_PARSE_FILE}.grm
	cp mlton-control.sml.in mlton-control.sml

mlton-bin: mlton-base
	echo -e "val _ = print \"EventML version ${VERSION} (built ${DATE2} on ${HOST} using MLton)\\\n\\\n\";" >> mlton-control.sml
	echo "val _ = slice ();" >> mlton-control.sml
	mkdir -p ${EVENTML_BIN_DIR}
	#nice --adjustment=19 mlton -verbose 1 -output ${EVENTML_BIN_DIR}/${BIN_NAME} sources.mlb
	${MLTON} -verbose 1 -output ${EVENTML_BIN_DIR}/${BIN_NAME} sources.mlb
	make clean-parse

bin: mlton-bin
