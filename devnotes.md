# Nemesis 

this code came from the glycogen project 

## TODO

generic+'s default case do not work properly DONE

removing the :any typetag should decrease code complexity DONE 

extracting default case from regular cases DONE 

### forking

how to represent a forked spec in the compile time registry ?
is an alias is enough ?
prototypes runtime atom could keep track of actual implementations 

### 3 modes of extension

extend : will only add implementations, no overiding of any sort, most safe (totally safe if no default impl)
tune : like extend with the ability to precise some implementations (if :coll implements g, we can extend g to :vec)  (can break existing code)
patch  : like tune with the ability to overide methods entirely (least safe)

extend and tune modes are just doing extra checks over the most permisive mode patch

some generics could disallow extension entirely

### 

Since we've introduced a runtime state that holds implementations at runtime, we could add some facilities
to wrap/upd behavior at the generic or implementation level
extract methods to build object on the fly (maybe not possible on JVM)

The expansion-state dynamic variable could go away in favor of fully functional 
The macro layer should always contains a call to the function that implements the macro, passing it {:env &env :form &form}

I could try to completly wrap defn behavior
I mean that if a generic has only default implementations it is just a normal function
It can become a generic if extended.
This way we enforce the convention of passing the object as first argument to all functions






