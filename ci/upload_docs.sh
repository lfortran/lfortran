#!/usr/bin/env bash

set -e
set -x

git_ref=${GITHUB_REF}
branch_name=${GITHUB_REF##*/}

if [[ ${git_ref:0:11} == "refs/tags/v" ]]; then
    # Release version
    dest_branch="master"
    deploy_repo="git@github.com:lfortran/docs.lfortran.org.git"
else
    # Development version
    dest_branch=${branch_name}
    deploy_repo="git@gitlab.com:lfortran/web/docs.lfortran.org-testing.git"
fi

mkdir ~/.ssh
chmod 700 ~/.ssh
ssh-keyscan gitlab.com >> ~/.ssh/known_hosts
ssh-keyscan github.com >> ~/.ssh/known_hosts

eval "$(ssh-agent -s)"

set +x
if [[ "${SSH_DEPLOY_KEY}" == "" ]]; then
    echo "Note: SSH_DEPLOY_KEY is empty, skipping..."
    exit 0
fi

ssh-add <(echo "$SSH_DEPLOY_KEY" | base64 -d)
set -x


D=`pwd`

mkdir $HOME/repos
cd $HOME/repos

git clone ${deploy_repo} docs-deploy
cd docs-deploy
rm -rf docs
mkdir docs
echo "docs.lfortran.org" > docs/CNAME
cp -r $D/site/* docs/

git config user.name "Deploy"
git config user.email "noreply@deploylfortran.com"
COMMIT_MESSAGE="Deploying on $(date "+%Y-%m-%d %H:%M:%S")"

git add .
git commit -m "${COMMIT_MESSAGE}"

git push origin +master:$dest_branch
