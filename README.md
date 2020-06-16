# AMPL / AMPLC

This contains the folders for AMPL and AMPLC

# What is AMPL? / What is AMPLC?

AMPL is a concurrent abstract machine for message passing language with support for TCP connections to clients.
AMPL's project files can be found in `AMPL/`.

AMPLC is the client for the networked AMPL connections which gives us the classic IntTerm and CharTerm
AMPLC's project files can be found in `AMPLC/`.

# How to use...
Type:
```
cd AMPLC/           # Change directory to AMPLC
stack install       # This will install the client which is required for AMPL
                    # Note: by default, stack will just compile and copy the executable,
                    # amplc, to ~/.local/bin/amplc (which should be in your path)
cd ..               # Go back a directory..
cd AMPL             # Change directory to AMPL
stack ghci          # Open GHCi
```
If GHCi asks you about 2 possible mains, just press `1` to choose the `main` function from AMPL.

In GHCi, we can run a few examples. All examples can be found in `AMPL/src/Tests.hs`.

For example, to test the parallel or, in GHCi, type:
```
parallelOrServiceTest
```
And provided `xterm` and `read` are installed (these are a terminal and a command
line utility to read input), two windows will pop up asking for a character as input.
In this program, `true` is denoted as the character `t` and `false` is any other character.

See the projects' individual `README.md` for more information i.e., look at `AMPL/README.md` or `AMPLC/README.md` for more information.
