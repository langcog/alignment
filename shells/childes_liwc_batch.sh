cd ../

for CORPUS in ~/Documents/CHILDES/ENG-NA-MOR/* 
 do
 #	if ! [ -d "CHILDES_liwc/ENG-NA-MOR/${CORPUS##*/}" ]; then
 		mkdir CHILDES_liwc/ENG-NA-MOR/"${CORPUS##*/}"
 		python3 ~/Projects/alignment/parsers/CHILDES_analysis3.py -C=${CORPUS##*/} -r -m=wordlists/liwc2007_converted.tsv
 		#echo "CHILDES_liwc/${CORPUS##*/}"
 #	fi
done
#for CORPUS in Brown
#do
#	
#done