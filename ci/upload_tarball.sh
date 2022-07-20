#!/usr/bin/env bash

set -e
set -x

deploy_repo_pull="https://github.com/lfortran/tarballs"
deploy_repo_push="git@github.com:lfortran/tarballs.git"

git_hash=$(git rev-parse --short "$GITHUB_SHA")
git_branch=${GITHUB_REF#refs/heads/}
tag=${GITHUB_REF/refs\/tags\//}


if [[ $tag == "" ]]; then
    # Development version
    dest_dir="testing1"
else
    # Release version
    dest_dir="testing2"
fi

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
git config user.email "noreply@deploylfortran.com"
COMMIT_MESSAGE="Deploying on $(date "+%Y-%m-%d %H:%M:%S")"

git add .
git commit -m "${COMMIT_MESSAGE}"

git show HEAD -p --stat
dest_commit=$(git show HEAD -s --format=%H)

if [[ ${git_branch} == "main" ]]; then
    echo "The pipeline was triggered from the main branch"
else
    if [[ $tag != "" && ${tag:0:1} == "v" ]]; then
        echo "The pipeline was triggered from a tag 'v*'"
    else
        # We are either on a non-master branch, or tagged with a tag that does
        # not start with v*. We skip the upload.
        echo "Not a main branch, not tagged with v*, skipping..."
        exit 0
    fi
fi

set +x
if [[ "${SSH_DEPLOY_KEY}" == "" ]]; then
    echo "Note: SSH_DEPLOY_KEY is empty, skipping..."
    exit 0
fi

ssh-add <(echo "$SSH_DEPLOY_KEY" | base64 -d)
set -x


git push ${deploy_repo_push} main:main
echo "New commit pushed at:"
echo "https://github.com/lfortran/tarballs/commit/${dest_commit}"

echo "The tarball is available from:"
echo "https://lfortran.github.io/tarballs/${dest_dir}/lfortran-${lfortran_version}.tar.gz"
