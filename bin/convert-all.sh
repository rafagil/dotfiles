#!/bin/bash

outdir=~/Downloads/

for filename in *.mov;
do
	convert.sh $filename
done
