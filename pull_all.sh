#!/bin/sh

keyfile=session.key

session=$(cat "$keyfile")

mkdir -p data

for arg in `seq 1 25`; do
	day=$(printf "%02d" $arg)
	printf "Fetching day $day... "
	curl -fs -H"Cookie: session=$session" "https://adventofcode.com/2017/day/$arg/input" > "data/day$day.in" && printf "done.\n" || printf "errored!\n"
done
