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
#  o Authors:     Vincent Rahli, Nicolas Schiper
#  o Affiliation: Cornell University
#  o Date:        24 May 2013
#  o File name:   Makefile
#

default: shadowdb aneris lib

.PHONY: shadowdb
shadowdb:
	(cd ShadowDB; ant)

.PHONY: aneris
aneris:
	(cd aneris; make bin)

aneris-lisp:
	(cd scripts; ./compile-aneris-lisp.sh)

.PHONY: lib
lib:
	(cd lib; tar -xvzf alldefs.tar.gz 2>/dev/null)

clean:
	rm jars/ShadowDB.jar

testdb:
	(cd scripts; ./startDbServer.sh 1 ../conf/conf_aneris_local.emlc stdalone INTERPRETED)

testrep1:
	(cd scripts; ./startDbServer.sh 1 ../conf/conf_aneris_local.emlc replicated INTERPRETED)

testrep2:
	(cd scripts; ./startDbServer.sh 2 ../conf/conf_aneris_local.emlc replicated INTERPRETED)

testbank:
	(cd scripts; ./startBankingClient.sh ../conf/conf_aneris_local.emlc read-only 50000 5 5 1)

start1:
	(cd scripts; ./startAneris1.sh local)

start2:
	(cd scripts; ./startAneris2.sh local)

start3:
	(cd scripts; ./startAneris3.sh local)

start4:
	(cd scripts; ./startAneris4.sh local)
