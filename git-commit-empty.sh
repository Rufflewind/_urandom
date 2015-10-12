#!/bin/sh
git init &&
git -c user.name=. -c user.email="<>" commit -m "" \
  --allow-empty --allow-empty-message --date="@0 +0000" &&
git filter-branch --env-filter \
  'export GIT_COMMITTER_DATE="@0 +0000" GIT_AUTHOR_NAME=. GIT_COMMITTER_NAME=.'
