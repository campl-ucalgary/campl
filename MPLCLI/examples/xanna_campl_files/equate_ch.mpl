
-- 

proc equateChannels :: | A, Neg(A) => A  =
    | a, neg_a => a' -> do
        plug
            neg_a, z => -> neg_a |=| neg z 
            => z -> halt z
            a => a' -> a |=| a'	 -- equating        
        -- close a'
        -- neg_a |=| neg a                  -- equating after negating

proc run =
    | => -> plug
        equateChannels(| a, neg_a => a2)
        a2 => -> halt a2
        => neg_a -> plug
            => neg_a, z -> neg_a |=| neg z 
            z => -> halt z
        => a -> halt a
