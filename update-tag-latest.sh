branch="$(git branch | grep \* | cut -d ' ' -f2)"
if [ "master" == "$branch" ]
then
        echo "Pushing tag 'latest' to HEAD in 3s"
        sleep 3
        git tag -d latest
        git push --delete origin latest
        git tag -a latest -m "current HEAD of master"
        git push origin latest
else 
        echo "Error: You must be on master branch to push the tag to HEAD."
fi