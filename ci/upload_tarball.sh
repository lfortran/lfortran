#!/usr/bin/env bash

set -e
set -x

deploy_repo_pull="https://github.com/lfortran/tarballs"
deploy_repo_push="git@github.com:lfortran/tarballs.git"

git_hash=$(git rev-parse --short "$GITHUB_SHA")
git_branch=${GITHUB_REF#refs/heads/}

dest_dir="testing"

lfortran_version=$(<version)

mkdir ~/.ssh
chmod 700 ~/.ssh
ssh-keyscan github.com >> ~/.ssh/known_hosts

eval "$(ssh-agent -s)"

D=`pwd`

mkdir $HOME/repos
cd $HOME/repos

git clone ${deploy_repo_pull} tarballs
cd tarballs/docs
mkdir -p ${dest_dir}
cp $D/dist/lfortran-${lfortran_version}.tar.gz ${dest_dir}/

python $D/ci/tarball_update_json.py ${dest_dir} ${lfortran_version} ${git_hash}

git config user.name "Deploy"
git config user.email "noreply@deploy"
COMMIT_MESSAGE="Deploying on $(date "+%Y-%m-%d %H:%M:%S")"

git add .
git commit -m "${COMMIT_MESSAGE}"

git show HEAD -p --stat
dest_commit=$(git show HEAD -s --format=%H)


set +x
if [[ "${SSH_DEPLOY_KEY}" == "" ]]; then
    echo "Note: SSH_DEPLOY_KEY is empty, skipping..."
    exit 0
fi

ssh-add <(echo "$SSH_DEPLOY_KEY" | base64 -d)
set -x


git push ${deploy_repo_push} master:master
echo "New commit pushed at:"
echo "https://github.com/lfortran/tarballs/commit/${dest_commit}"

echo "The tarball is available from:"
echo "https://lfortran.github.io/tarballs/${dest_dir}/lfortran-${lfortran_version}.tar.gz"
