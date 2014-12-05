#!/bin/bash

# Parameters: [-hb] [Number_of_nodes]

unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
     exename=erl
else
    exename='start werl.exe'
    #exename='erl.exe'
fi

hbhost="127.0.0.1"
if [[ "$1" == "-hb" ]]; then
    host=`hostname`
    hostparts=(${host//./ })

    hbhost=${hostparts[0]}h
    for (( i=1; i<${#hostparts[@]}; i++ ));
    do
        hbhost=$hbhost.${hostparts[$i]}
    done
fi

# Node name
ndnam="erlhlc0"
node="$ndnam@$hbhost"
node_name="-name $node"

HBFmt=$(echo $hbhost | sed 's/WKS\([0-9]\{3\}\)\(h.*\)/\1-\2/')
HBPrts=(${HBFmt//-/ })
OtherHosts="[]"
if [[ "${#HBPrts[@]}" == "2" ]]; then
    LNum=${HBPrts[0]}
    HSuff=${HBPrts[1]}
    for i in $(seq -f "%03g" 10 15)
    do
        if [[ "$i" -ne "$LNum" ]]; then
            if [[ $OtherHosts == "[]" ]]; then
                OtherHosts="["
            else
                OtherHosts=$OtherHosts","
            fi
            OtherHosts=$OtherHosts"'$ndnam@WKS$i$HSuff'"
        fi
    done
    OtherHosts=$OtherHosts"]"
fi

# Cookie
cookie="-setcookie erlhlc"

# PATHS
paths="-pa"
paths=$paths" $PWD/ebin"
paths=$paths" $PWD/deps/*/ebin"

start_opts="$paths $cookie $node_name"

# DDERL start options
echo "------------------------------------------"
echo "Starting ERLHLC (Opts)"
echo "------------------------------------------"
echo "Cookie    : $cookie"
echo "EBIN Path : $paths"
echo "Command   : $exename $start_opts -s erlhlc"
echo "OtherHost : $OtherHosts"
echo "------------------------------------------"

# Starting erlhlc
$exename $start_opts -s erlhlc -eval "[apply(net_adm,ping,[N])||N<-$OtherHosts]."

if [[ "$1" == "-hb" ]]; then
    nodes=$2
else
    nodes=$1
fi

if [[ "$nodes" -le "0" ]]; then
    nodes=0
fi

for (( c=1; c<=$nodes; c++ ))
do
    node_name="-name erlhlc$c@$hbhost"
    start_opts="$paths $cookie $node_name"
    $exename $start_opts -s erlhlc -eval "apply(net_adm,ping,['$node'])."
done
