Emails regarding the time out. At the top, we have the most recent email, and at the bottom wehave the oldest email.

# Cockett
Just wanting to get the the primitive for timing as clear as possible:

a) Let us call it a "timer": I think we just need one primitive which has the following type

timer:: Int | Get((),Top)  => 

b) calling 
      timer(2000|a) 
will cause the timer process to wait 2000 milliseconds (is this an appropriate unit?) and then put a "unit" response on the channel a.  This should be a builtin function.  I am hoping that this primitive suffices to get all the behaviours we need ...

c) One can then define (a slight rewriting of earlier code)

coprotocol  C => TimeOut =
       Timer:: C => Put(Int|C (x) Get((),Top))
        EndTimer:: C => Top

timeouts:: TimeOut => 
         = in => 
         do hcase in of 
                 Timer 
                      do get n on in
                            split in into in_0, in_1
                            timer(n|in_1)
                            timeouts(in0 =>)
                  EndTime 
                        do close in

# Cockett
Ooops!   You are right forking has to have disjoint channels.  This was, however, a typo and should have been ...

fork b as 
    b0 with in0 -> b0 |=| in1 
    b1 with in1 -> do 
        get _ on in1   % accept the beep 
        close in1        % kill the thread
        halt b1

I am sure that this is not the only problem!  However, I think in principle we can do a timeout.  

The question is what primitive is  needed in order to have a timeout ...  I am thinking we  need 
a function 

timeout:: Int |Get((),TopBot) => 

Which takes in an integer (length of timeout) and gives a beep by outputting ...  given this one can build timeout interactions.

-robin

# Jared
Interesting! I don't think the kill_timeout will quite typecheck, but we can talk about this more on Thursday 🙂.

Although, just to clarify -- can the 2 processes generated with a fork instruction share a channel between them? In kill_timeout we have (with some extra annotation of how the channels are being shared) :
               
fork b as
    b0 with in1 -> b0 |=| in1
    b1 with in1 -> do
        get _ on in1   % accept the beep
        close in1        % kill the thread
        halt b1

where in1 is shared between the forked channel b.

I thought that fork does not allow this behavior.

Have a nice night!

# Cocket
I think the idea of putting the time out on a thread which  one destroys when the race is lost by the timer can be implemented in MPL.  Here is my attempt ... would be nice to get this going in some form eventually! (This probably will not type 🙂)!!!

coprotocol  C => TimeOut =
       Timer:: C => Put(Int|C (x) Get((),Top))
        EndTimer:: C => Top

proc timed_terminal:: TimeOut  => Terminal 
                        = in => out 
do
    hput Timer on in
    put 20 on in 
    hput TGet on out     % ready to receive input from terminal
    split in into
        (in0, in1) ->  race 
                 in1 ->   get _ on in1     % accept the "beep" from the timer
                              get _ on out     % get the input from the terminal (ignore it!)
                              hput TPut on out  
                              put "Timed out!" on out
                              hput TEnd on out
                              hput EndTimer on in0 
                              close in0; close in1; halt out        % end the terminal interaction and close all channels
                 out ->  get _ on out     % get the output from the terminal (ignore it! Could do something!)
                             plug 
                                kill_timeout(in0,in1 => in2)                        % kill the timer thread
                                timed_terminal(in2 => out)        % recall the timed terminal

proc kill_timeout:: TimeOut, Get((),Top) => TimeOut 
                     = in0, in1 => out 
 do
       plug  do fork b as 
                           b0 -> b_0 |=| in1 
                           b1 -> get _ on in1   % accept the beep 
                                     close in1        % kill the thread
                                     halt b1

                 split b into
                       (b0,b1) -> close b1      %  b1 is or type Top ... (adding a dummy channel on which to fork)
                                          b0 |=| out  

A possibly neater syntax for killing would be 

proc kill_timeout:: TimeOut, Get((),Top) => TimeOut 
                     = in0, in1 => out 
 do
       fork b as 
             b0 -> b_0 |=| in1 
             * -> get _ on in1   % accept the beep 
                     halt in1        % kill the thread

where we allow implicit forking onto a unit channel Top or TopBot: this seems like something useful to have!

-robin
