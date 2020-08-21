#!/usr/local/bin/bash
shopt -s nullglob
for file in *.ampl; do
	filename="${file%.*}"
	printf "[asm] Compiling $file... "
	if stack exec amplasm -- "$file" "$filename.amplasm"; then
		printf "Success!\n"
	fi
done
