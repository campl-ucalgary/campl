#!/usr/local/bin/bash
shopt -s nullglob
for file in *.ampla; do
	filename="${file%.*}"
	printf "[asm] Compiling $file... "
	if stack exec amplasm -- "$file" "$filename.amplm"; then
		printf "Success!\n"
	fi
done
