git pull &&
git rm -f status/*.md &&
git commit status -m "Delete old release status" &&
git push
