#!/bin/bash

# Converting multiple bed into vcf


awk -F'\t' '{print $1,$2,"rs1208179@C@T@14.92@LowGQ;LowGQX;LowDP@AC=2;AF=1.00;AN=2;DP=2;Dels=0.00;FS=0.000;HRun=0;HaplotypeScore=0.0000;MQ=37.00;MQ0=0;QD=7.46;SB=-40.01@GT:AD:DP:GQ:PL:VF:GQX@1/1:0,2:2:3.01:45,3,0:1.000:3",$4}' ANNOTATION_INDEX.DGV> temp0

sed -e "s/@/\t/"   temp0  | sed -e "s/@/\t/" |sed -e "s/@/\t/" |sed -e "s/@/\t/" |sed -e "s/@/\t/" |sed -e "s/@/\t/" | sed -e "s/@/\t/" |sed -e "s/@/\t/" |sed -e "s/@/\t/" |sed -e "s/@/\t/" |sed -e "s/@/\t/" | sed -e "s/ /\t/"  | sed -e "s/ /\t/" > tempxx 


sed -e "s/ /\t/" tempxx > epimutation_tot.vcf