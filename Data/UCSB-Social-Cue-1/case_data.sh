# Backup
$ tar -czvf P9E3_v1.tar.gz P9E3_v1*.txt

# Convert from DOS to Unix
$ a=`ls *.txt`
$ for f in $a; do dos2unix $f; done

# Ensure each file has one and only one path and header line
$ ls *.txt | wc -l
$ grep Users *.txt | wc -l
$ grep Subject *.txt | wc -l

# Collect the data in one file
$ grep Subject P9E3_v1-1-1_edat2.txt > case-data.csv
$ for f in $a; do cat $f | grep -v Users | grep -v Subject >> case-data.csv; done

# Use emacs to replace tab separators with commas
