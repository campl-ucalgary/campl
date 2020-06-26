
rm output.ampl
stack exec amplasm -- $1 output.ampl
stack exec ampl -- output.ampl

