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

# Magic Eye
The idea is that you pulll resources from somewhere... 

a number of channels and distributed a task
and you poll a channel.. if it polls in a certain amount of time you give it another task... 
If it runs out you poll out the task. and keep doing it forever...

# ping pong
two messages (ping and pong)

The main program is just going to ping and pong... 
The two semaphores (to put such a common thing)


The receiver just gets stuff


What we're trying to do is force ping and pong (into ping and pong) 

So, there is a connection to the main console (and a connection between the two things).... 

Well you shouldn't be able to really do that in MPL! Cyclic code is kinda bad

# Notes on racing..
Okay you can request another terminal and it can break off..

Protocols have leafs, and you can request that you can break up the leaves...

How about you have a list.. arranged by the Console
    - At the console, we can ask for another terminal; and what it does it pops another pair on the list

    - the list is a protocol

# Message board
* Make the console the spawner... and a sepearte string termianl for the messages

# Booking system
* Theater of 5 seats 
* The console is just for opnening terminals
* and we write all to some central terminal?

* Someone books, then we refresh... i.e., display what the state of the booking is now... 

1. race for the right to get hold of the memory cell.. 
2. the race gets the memory cell up to the leaf
3. then, it has to go right back to the base again when it does that 
    (when we move it to the leaf, we need to generate the path back..., so it's easy to return the state to the root)
    


