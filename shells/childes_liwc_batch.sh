cd ../

for i in `seq 11 20`;
do
	for DIR in ~/Documents/CHILDES/*
	do
		for CORPUS in $DIR/*; do
			#echo "CHILDES_liwc/${DIR##*/}/${CORPUS##*/}"
			if ! [ -d CHILDES_liwc/${DIR##*/}/${CORPUS##*/} ]; then
				mkdir "CHILDES_liwc/${DIR##*/}/${CORPUS##*/}"
			fi

			python3 ~/Projects/alignment/parsers/CHILDES_analysis3.py -C=${DIR##*/}/${CORPUS##*/} -r -m="wordlists/liwc2007_shuffled$i.tsv"
			#echo $CORPUS
		done
	done
done