awk '{ total += $1 ; count++ } END { print total/count }' $1
