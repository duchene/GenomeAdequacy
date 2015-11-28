#!/bin/bash

files=`ls *phy`

for i in $files; do 
    echo $i
    Rscript ../runGTRG.R $i
done
