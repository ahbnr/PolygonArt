# pre-commit.sh
# based on: http://codeinthehole.com/tips/tips-for-using-a-git-pre-commit-hook/
git stash -q --keep-index
stack test
RESULT=$?
git stash pop -q
[ $RESULT -ne 0 ] && exit 1
exit 0
