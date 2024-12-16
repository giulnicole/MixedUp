#!/bin/bash

# In the folder where the bed are placed

for file in *.bed; do                                                                                                                                                                                                                                                                      grep -v -h '^#' $file > "$(basename "$file" .vcf).ID"; 
done


ls *.ID|xargs -I% sed -i 's/$/\t%/' %

cat *.ID > temp0

awk -F'\t' '{print $1"-"$2"-"$3}' temp0 > temp1

awk -F '\t' '{print $1}' temp1 | sort | uniq -c | sort -nr > temp1x

awk -F' ' '{print $2, $1}' temp1x > temp1xx


sort -k1 temp1 > temp1.1
sort -k1 temp1xx > temp1xx.1


join -a 1 -a 1 temp1.1 temp1xx.1 > temp1.2a
awk -F"\t" '!_[$1,$2,$3]++' temp1.2a > temp1.2
paste temp1 temp0 > temp3

sort -k1 temp3 > temp4
sort -k1 temp1.2 > temp5

join -a 1 -a 1 temp4 temp5 | awk -F' ' '{print $1,$2,$3,$4,$6,$5,$7}' > temp1000
awk -v OFS="\t" '$1=$1' temp1000 >  MULTIPLE.DGV

awk -F"\t" '!_[$1,$2,$3,$4]++' temp0 > ANNOTATION_INDEX.DGV

rm -f, yes | rm *.ID
rm -f, yes | rm temp*