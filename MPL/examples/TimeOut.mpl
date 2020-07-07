coprotocol  C => TimeOut =
    Timer:: C => Put(Int|C (*) Get((),Top))
    EndTimer:: C => Top

protocol Terminal => C =
    TGet :: Get(Int|C) => C
    TPut :: Put(Int|C) => C
    TClose :: Bot => C

proc timed_terminal:: TimeOut  => Terminal 
    = in => out -> do
        hput Timer on in
        put 20 on in 
        hput TGet on out     % ready to receive input from terminal
        split in into (in0, in1) 
        race 
            in1 -> do
                get _ on in1     % accept the "beep" from the timer
                get _ on out     % get the input from the terminal (ignore it!)
                hput TPut on out  
                put "Timed out!" on out
                hput TEnd on out
                hput EndTimer on in0 
                close in0; close in1; halt out        % end the terminal interaction and close all channels
            out -> do
                get _ on out     % get the output from the terminal (ignore it! Could do something!)
                plug in2
                    kill_timeout(in0,in1 => in2)      % kill the timer thread
                    timed_terminal(in2 => out)        % recall the timed terminal

proc kill_timeout:: TimeOut, Get((),Top) => TimeOut 
    = in0, in1 => out -> do
        plug b
            with in0,in1 -> do 
                fork b as 
                    % b :: TimeOut (*) TopBot
                    b0 with in0 -> b0 |=| in0 
                        % b0 :: TimeOut

                    b1 with in1 -> do 
                        get _ on in1   % accept the beep 
                        close in1      % kill the thread
                        halt b1        % b1 :: TopBot

            with out -> do
                split b into (b0,b1) 
                close b1             % b1 :: TopBot
                b0 |=| out           % b0 :: TimeOut


A possibly neater syntax for killing would be 

proc kill_timeout:: TimeOut, Get((),Top) => TimeOut 
                     = in0, in1 => out 
 do
       fork b as 
             b0 -> b_0 |=| in1 
             * -> get _ on in1   % accept the beep 
                     halt in1        % kill the thread


