-- Using a module without importing it.


include "Second.mpl" : M1


fun f =
    a -> Second.a(f)
