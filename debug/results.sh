echo "True True: "
head -$1 results.csv | grep -c "True.*True"
echo "True False: "
head -$1 results.csv | grep -c "True.*False"
echo "False True: "
head -$1 results.csv | grep -c "False.*True"
echo "False False: "
head -$1 results.csv | grep -c "False.*False"
echo "----------------------"
echo "True True: "
tail -n $1 results.csv | grep -c "True.*True"
echo "True False: "
tail -n $1 results.csv | grep -c "True.*False"
echo "False True: "
tail -n $1 results.csv | grep -c "False.*True"
echo "False False: "
tail -n $1 results.csv | grep -c "False.*False"