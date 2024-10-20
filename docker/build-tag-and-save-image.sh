#!/bin/bash

branch_suffix=""
if [[ ! "$(git status)" =~ "nothing to commit, working tree clean" ]]; then
    branch_suffix="-dirty"
fi

# Build
docker build .. -f Dockerfile --tag osklunds/http-image-server-dev || exit 1

# Tag
branch=$(git rev-parse --abbrev-ref HEAD)
date_and_commit_hash=$(git show --no-patch --no-notes \
                           --pretty='%cd--%h' \
                           --date=format:'%Y-%m-%d--%H-%M-%S' -s HEAD)

echo $date_and_commit_hash

file_name="http-image-server--$branch$branch_suffix--$date_and_commit_hash"
tag="osklunds/$file_name"

echo "Tagging with tag '$tag'"

docker tag osklunds/http-image-server-dev "$tag" || exit 1

# Save
out_path="saved_images/$file_name.tar"
echo "Saving to '$out_path'"

mkdir -p saved_images
docker image save "$tag" --output "$out_path"
