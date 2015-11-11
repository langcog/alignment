module load python/3.3.2

for CORPUS in Brown Kuczaj Manchester Providence Suppes Thomas
do
	python CHILDES_analysis3.py -C=$CORPUS -r -m=wordlists/wordbank_50_100_20mos.tsv
done