#!/bin/sh
set -e

test "$(git rev-parse --abbrev-ref HEAD)" = "master"

cabal new-run blog rebuild
git stash push -a _site/
git stash push -a

finale() {
	git checkout master
	git stash pop || true
}

trap finale EXIT

git checkout pages
git rm -rf _site/
git stash pop stash@{1}
git add _site/
git commit -m .
git push

