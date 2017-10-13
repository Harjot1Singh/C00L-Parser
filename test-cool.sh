#!/usr/bin/env bash

for file in tests/*
do
    echo -e "\nRunning parser on ${file}"
    python src/coolparser.py ${file}
done