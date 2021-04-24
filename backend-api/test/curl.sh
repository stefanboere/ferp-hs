#!/usr/bin/env bash

curl -i -s -X GET -G \
 'http://localhost:3005/blogs' \
 -H "accept: text/csv" \
 -H 'Authorization: Bearer '
