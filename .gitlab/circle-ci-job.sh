# Circle CI "backend" for Gitlab CI
# =================================
#
# Usage example:
#   .gitlab/circle-ci-job.sh validate-x86_64-linux
#
# It currently requires the following environment variables to
# be set:
#
# - CI_RUNNER_ID, CI_JOB_ID, CI_COMMIT_SHA, set automatically,
#   as per: https://docs.gitlab.com/ce/ci/variables/
#
# - CIRCLECI_TOKEN, which should be set as a Gitlab
#   CI "variable", as per:
#   https://docs.gitlab.com/ce/ci/variables/README.html#variables
#
# - SSH_PRIVATE_KEY, variable set in the gitlab interface, as per:
#   https://docs.gitlab.com/ce/ci/ssh_keys/#ssh-keys-when-using-the-docker-executor
#   This script itself doesn't actually need that one, but it is
#   needed for the .gitlab-ci.yml script.

#!/usr/bin/env sh

set -e

[ $# -gt 0 ] || (echo You need to pass the Circle CI job type as argument to this script; exit 1)
[ ${CI_RUNNER_ID:-} ] || (echo "CI_RUNNER_ID is not set"; exit 1)
[ ${CI_JOB_ID:-} ] || (echo "CI_JOB_ID is not set"; exit 1)
[ ${CI_COMMIT_SHA:-} ] || (echo "CI_COMMIT_SHA is not set"; exit 1)

GITHUB_ORG="ghc"
GITHUB_PROJECT="ghc-diffs"
GITHUB_BRANCH="gitlab/${CI_RUNNER_ID}/${CI_JOB_ID}"

# the first argument to this script is the Circle CI job type:
# validate-x86_64-linux, validate-i386-linux, ...
CIRCLE_JOB=$1

git remote add gh git@github.com:${GITHUB_ORG}/${GITHUB_PROJECT} || echo "gh remote already present"
git checkout -b ${GITHUB_BRANCH} || true # if we've already done this before
git push gh ${GITHUB_BRANCH} || true # if we've already done this before

BODY="{ \"revision\": \"${CI_COMMIT_SHA}\", \"build_parameters\": { \"CIRCLE_JOB\": \"${CIRCLE_JOB}\" } }"
RESP=$(curl -X POST -H "Content-Type: application/json" -d "$BODY" \
	    https://circleci.com/api/v1.1/project/github/${GITHUB_ORG}/${GITHUB_PROJECT}/tree/${GITHUB_BRANCH}?circle-token=${CIRCLECI_TOKEN})
build_num=$(echo $RESP | jq '.build_num')

echo Circle CI build number: $build_num
echo Circle CI build page: https://circleci.com/gh/${GITHUB_ORG}/${GITHUB_PROJECT}/$build_num

outcome="null"
while [ "$outcome" == "null" ]; do
    sleep 30s
    STATUS_URL="https://circleci.com/api/v1.1/project/github/${GITHUB_ORG}/${GITHUB_PROJECT}/${build_num}?circle-token=${CIRCLECI_TOKEN}"
    STATUS_RESP=$(curl -s $STATUS_URL)
    if [ $? -eq 0]; then
       outcome=$(echo $STATUS_RESP | jq '.outcome')
    else
	echo "curl failed:"
	echo $STATUS_RESP
    fi
done

if [ "$outcome" == "success" ]; then
    echo The build passed && exit 0
else
    echo The build failed && exit 1
fi
