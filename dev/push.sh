git pull &&
git rm -f status/*.txt &&
git commit status -m "Delete old release status" &&
git push
