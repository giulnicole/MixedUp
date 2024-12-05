# Split a vcf in 1 vcf per each chromosome

vcf_in=/home/user/Desktop/whole_chromosomes.vcf

vcf_out_stem=/home/user/Desktop/by_chrom

for i in {1..22}
do
bcftools view ${vcf_in} --regions ${i} -o ${vcf_out_stem}_${i}.vcf.gz -Oz
done