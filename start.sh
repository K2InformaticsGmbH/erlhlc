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
node="erlhlc0@$hbhost"
node_name="-name $node"

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
echo "------------------------------------------"

# Starting erlhlc
$exename $start_opts -s erlhlc

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
