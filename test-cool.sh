#!/usr/bin/env bash

for file in tests/*
do
    echo -e "Running parser on ${file}\n"
    python src/coolparser.py ${file}
done