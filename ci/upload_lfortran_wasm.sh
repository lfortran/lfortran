#!/usr/bin/env bash

set -e
set -x

deploy_repo_pull="https://github.com/lfortran/wasm_builds.git"
deploy_repo_push="git@github.com:lfortran/wasm_builds.git"

git_hash=$(git rev-parse --short "$GITHUB_SHA")
git_ref=${GITHUB_REF}
dest_dir="release"

lfortran_version=$(<version)

mkdir ~/.ssh
chmod 700 ~/.ssh
ssh-keyscan github.com >> ~/.ssh/known_hosts

eval "$(ssh-agent -s)"

D=`pwd`

mkdir $HOME/repos
cd $HOME/repos

git clone ${deploy_repo_pull} wasm_builds
mkdir -p wasm_builds/docs/${dest_dir}/${git_hash}
cd wasm_builds/docs
cp $D/src/bin/lfortran.js ${dest_dir}/${git_hash}/lfortran.js
cp $D/src/bin/lfortran.wasm ${dest_dir}/${git_hash}/lfortran.wasm
cp $D/src/bin/lfortran.data ${dest_dir}/${git_hash}/lfortran.data

echo "$git_hash" > ${dest_dir}/latest_commit # overwrite the file instead of appending to it
python $D/ci/wasm_builds_update_json.py ${dest_dir} ${lfortran_version} ${git_hash}

git config user.name "Deploy"
git config user.email "noreply@deploylfortran.com"
COMMIT_MESSAGE="Deploying on $(date "+%Y-%m-%d %H:%M:%S")"

git add .
git commit -m "${COMMIT_MESSAGE}"

git show HEAD -p --stat
dest_commit=$(git show HEAD -s --format=%H)

if [[ ${git_ref} == "refs/heads/main" ]]; then
    echo "The pipeline was triggered from the main branch"
else
    if [[ ${git_ref:0:11} == "refs/tags/v" ]]; then
        echo "The pipeline was triggered from a tag 'v*'"
    else
        # We are either on a non-main branch, or tagged with a tag that does
        # not start with v*. We skip the upload.
        echo "Not a main branch, not tagged with v*, skipping..."
        exit 0
    fi
fi

set +x
if [[ "${SSH_PRIVATE_KEY_WASM_BUILDS}" == "" ]]; then
    echo "Note: SSH_PRIVATE_KEY_WASM_BUILDS is empty, skipping..."
    exit 0
fi

ssh-add <(echo "$SSH_PRIVATE_KEY_WASM_BUILDS" | base64 -d)
set -x


git push ${deploy_repo_push} main:main
echo "New commit pushed at:"
echo "https://github.com/lfortran/wasm_builds/commit/${dest_commit}"
