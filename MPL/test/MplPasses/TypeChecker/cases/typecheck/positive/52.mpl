protocol Turn( | M ) => U =
    Up :: U => U
    Turn :: M (*) (Neg(M) (+) U)  => U

proc branch = 
    | below => above -> hcase below of
        Up -> do    
            plug 
                branch( | below => nabove)
                nabove => above -> nabove |=| above
        
        Turn -> do
            split below into mem,negmemandnbelow
            fork negmemandnbelow as
                negmem -> negmem |=| neg mem
                nbelow -> branch( | nbelow => above)


