#!/bin/bash

# Parameters:  Number_of_nodes

if [[ "$unamestr" == 'Linux' ]]; then
     exename=erl
else
    exename='start werl.exe'
    #exename='erl.exe'
fi

# Node name
node="erlhlc0@127.0.0.1"
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
echo "------------------------------------------"

# Starting dderl
$exename $start_opts -s erlhlc

nodes=0
if [[ "$1" -gt "0" ]]; then
    nodes=$1
fi

for (( c=1; c<=$nodes; c++ ))
do
    node_name="-name erlhlc$c@127.0.0.1"
    start_opts="$paths $cookie $node_name"
    $exename $start_opts -s erlhlc -eval "apply(net_adm,ping,['$node'])."
done
