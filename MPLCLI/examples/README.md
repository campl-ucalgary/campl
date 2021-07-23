# Examples
This folder contains some more significant examples of some MPL programs.

## Tic tac toe
A classical tic tac toe game meant to be played by two different players.

Remarks.
- This uses a memory cell to pass the game state between the two players

TODO: Automatic player to replace the person (do this in checkers later too sometime)
    some modularity with the program. 

## Halma
A four player game where players try to get all their pieces to the other side..

Remarks.
- This generalizes the memory cell for four players. 

## Message board
As simple message board for people to dump messages on. The idea is as follows:

- two terminals open up, and

- each of the terminals may dump messages (without doing a get/put wait on each
  other) on the original console.

Remarks.
- This uses a race and the id command to allow both parties to dump stuff the
  the same terminal


TODO:
## Reaction based games...

### Typing test

### Snap
    - (TODO: need random then..)
    - deal out a whole bunch of cards... 
    - If you both turn over the same face card, then first person to call snap doesn't hvae to pick up the card.
    - objective: get rid of all cards..



# Semaphore..
- People asking for temrinals and demanding them..
- Bounded resources... After a certain amount of time, the resource becomes free.. 
- WE could do this byliterally gettin gthe person to type into a temrianl that a resource is free.

- There are only 2 interfaces for which a process can use. (producer and consumer)


- Either the semaphore returns a value or waits (block itself).. 


- 4 memory cells (and allocate the memory cells) an d orf course the proeses that pick them up havej to put them down...
    And when they put them down, they become avaiblab,e and they can be allocated to someone who wants them.


- More terminals (dynamically)
- Timeouts
- random number genrator

