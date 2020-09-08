#!/usr/local/bin/bash
tmpfile=$("./tmpfile.amplm")
stack exec amplasm -- $1 $tmpfile
stack exec ampl -- $tmpfile
rm "./tmpfile.amplm"
