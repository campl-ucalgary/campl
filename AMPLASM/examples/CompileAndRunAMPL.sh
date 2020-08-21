#!/usr/local/bin/bash
tmpfile=$(mktemp /tmp/output-XXXXXXXXXX.ampl)
stack exec amplasm -- $1 $tmpfile
stack exec ampl -- $tmpfile
rm "$tmpfile"
