#!/usr/local/bin/bash
shopt -s nullglob
for file in *.amplm; do
	filename="${file%.*}"
	printf "[bytecode] Compiling $file... "
	if stack exec amplm2amplb -- "$file" "$filename.amplb"; then
		printf "Success!\n"
	fi
done
