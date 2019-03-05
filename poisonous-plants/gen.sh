#! /bin/sh

OUTPUT_PATH=in6 ./testgenerator
OUTPUT_PATH=pout ./solution < in6
OUTPUT_PATH=pout ./solution1 < in6

