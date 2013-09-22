#!/bin/sh

if [ -n "$1" ]
then
	N=`printf "problem-%03d" $1`
	cabal run $N
else
	echo "Usage: run.sh <problem-number>"
fi
