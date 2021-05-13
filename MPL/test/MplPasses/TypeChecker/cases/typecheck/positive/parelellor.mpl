data 
    MyBool -> S = 
        MyTrue :: ->  S
        MyFalse :: ->  S

fun seqor :: MyBool, MyBool -> MyBool =
    MyFalse, MyFalse -> MyFalse
    _, _ -> MyTrue

proc parellelor :: | Put(MyBool | TopBot), Put(MyBool| TopBot) => Put(MyBool| TopBot) =
    | ach, bch => dch -> do
        race 
            ach -> do
                get b on ach
                case b of
                    MyTrue -> do
                        put b on dch
                        get _ on bch
                        close ach
                        close bch
                        halt dch
                    MyFalse -> do
                        get nb on bch
                        put seqor(nb,b) on dch
                        close ach
                        close bch
                        halt dch
            bch -> do
                get b on bch
                case b of
                    MyTrue -> do
                        put b on dch
                        get _ on ach
                        close ach
                        close bch
                        halt dch
                    MyFalse -> do
                        get nb on ach
                        put seqor(nb,b) on dch
                        close ach
                        close bch
                        halt dch

