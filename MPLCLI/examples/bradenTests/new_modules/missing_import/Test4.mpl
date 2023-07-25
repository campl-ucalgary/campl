-- import inside 'defn-where'





defn
    fun f =
        a -> a

    include Second
where
    fun g =
        a -> a


