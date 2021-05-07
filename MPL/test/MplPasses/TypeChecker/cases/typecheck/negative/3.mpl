defn
    fun fun0 :: A -> B =
        a -> fun1(a)
    fun fun1 :: B -> B =
        a -> fun0(a)
