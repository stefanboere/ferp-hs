#!/usr/bin/env bash

curl -s -X GET -G \
 'http://localhost:3005/blogs' \
 -H "accept: text/csv"
