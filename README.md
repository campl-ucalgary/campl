# MPL project

# What's what?
`AMPL/` is the abstract machine..
`AMPLASM/` is the assembler for writing code in the abstract machine...
`AMPLC/` is the client for the abstract machines' service channels.
`CMPL/` (Core Mpl) is the Haskell data type which will compile down to a text file that can be processed by AMPLASM
`MPL/` is the compiler front end for the abstract machine
`mpl-lib/` was an attempt to generalize some of the common functions between the assembler and the front end, but this should be deprecated in the future i.e., it was a terrible idea.

`Timeout.md` includes notes on the timeout function which was never implemented yet.

# How to use..
Well, this thing is unfinished -- so it can't be really used yet. 

The front end has no infrastrucutre for actually trying to compile a file put in. For now, it is meant to be just played with in GHCi then after all those cases are sorted out, the plan was to make it a complete command line application people can actually use.. But for now, we cannot do anything with it aside from play with expressions in GHCi.

Although, the assembler and abstract machine work (mostly, if we take away the rough edges)

Below, is a TODO list with everything that needs to be done... Some of them are optional (i.e., would be nice) but others need to be done to get code pushing from front end to abstract machine.

# TODO List for Documentation 
- Write up the type equations for all the expressions... Some of the type equations used differ from the one's in Prashant's thesis so the changes should be documented.
- Edit CMachine document(?) (Mainly for Niran)
- Write up documentation for the MPL, AMPLASM,CoreMpl... The code is a little confusing for newcomers! 

- And finally, when this is all written up, there should be documentation with how to use everything...

# TODO List for Code
Unfortunately, this is unfinished!
We have lots to do still although this is fairly detailed of what needs to be done, so just tackling this stuff one piece at at time would probably be best.
Starting bottom up would be a good idea.

For AMPL, we need to:
- Add a time outprimitive (see Timeout.md for more details)
- Add support for string services (Previously, there was some confusion on how to do this because the C machine is concerned about the physical hardware representation of strings. I think the solution we are going to go with is just hard code certain constructors to be a string (like what Prashant did))
- Removing machine state logging (Currently, it logs each machine state out to a file.. this is slow.)
- Change the machine to use lenses (instead of using the crappy ghetto reimplementation of lenses it currently uses) 
- Change services to be a fold over some data type -- much more general and allows extensibility for people to define their own services

For AMPLC (terminal client for AMPL), we need to:
- Polish rough edges -- unnecessary debug information of passwords and IP addresses is dumped to the console
- Does not support strings! 

For AMPLASM (Assembler for an AMPL assembly language), we need to:
- write a pretty printer for error messages (currently just uses the Show instance for the error messages data type)
- Review how it handles non exhaustive patterns (I think it does this in reverse order for now? this needs some revisting... But, the frontend compiler should insert the error instances in..)
- Change the code so it uses lenses instead of the ghetto reimplementation of classy prisms (cleaner!)

For Core MPL (strange Haskell data type used to translate the front end AST to AMPLASM), we need to:
- Test if this actually works... At the time, the author never finished to front end and never had a chance to see if it really did generate ASM code properly.
- Get rid of Core MPL and just use the MPL AST (from the front end) and compile that straight down to assembly since it is an extensible AST with pretty printers already.

For MPL (compiler front end), we need to:
- Type checking / renaming is missing support for built in types (still! The author at the time got sick between the end of the work semester and before school and couldn't quite finish this up! Although, only the ``easy built in types" cases are left, so it should not be too bad to fill in these remaining cases)
  - Remark: building codata records may need to be revisited... i.e., writing (D := a,b,c -> ... ) or (D := -> somefun) may both be accepted -- i.e., given records of higher order functions, we may either explictly write the lambda or directly write the function in as if it is partially applied (but we CANNOT partially apply a function in general). This needs more thought / investigation in general.
  - Test cases for higher order unfolds of codata need to be put in.

- Kind checking (in the type checking phrase) needs to be revisited
  - I would like to see a proper kind checking system in the future (checking the types of types!). We would have 4 kinds:
    - ``=>" (concurrent arrow)
    - ``->" (sequential arrow)
    - ``+" (concurrent type?)
    - ``*" (sequential type?)
   And hopefully alllow higher kinded data being passed around. This is pretty much necassary to think about if we decide to add a type class system in the future...

- Parsing / renaming / typechecking NEED a pretty printer for error messages.. Certain error messages will cause the system to INFINITE LOOP because it stores the graph of data types (data, codata, protocol, coprotocol) and simply uses the show instance to print it which indeed has cycles within it.

- Parsing needs revisting but is manageable for now...
  - In the future, I would like to remove the BNFC dependency.. BNFC does not allow you to get the position of a token which is also a layout keyword (i.e., we cannot lex a token as both a ``position token" and a ``layout" word). In particular, when giving error messages with the keywords ``race" and ``plug", we cannot know the position of those commands because they are layout keywords. Possible alternatives include: writing the lex / happy file up, or using a monadic parser combinator library.
  - Although, I think the monadic parser combinator library option is the better option because there are keywords that are both a regular keyword and a layout keyword -- but choosing which kind of keyword it is cannot be known at lex time. For example ``=>" is normally not a layout keyword, but in the pressence of an unfold expression (as given in Prashant's thesis) it is. Currently, the work around is to use ``of" in place of ``=>" as the layout keyword.
  - Also, I would like to support both the syntax ``(D := a)" and ``(D := -> a)" when building records of codata in the future.. Currently, only the latter is accepted because there are reduce/shift errors when dissambugating the commas in the patterns and commas between defining the destructors. This would be a fairly substantial change to the grammar (which a change to a monadic parser library would justify this change!)
  - Note that defining your own operators is not allowed either! In the future, this should be changed... Although, I think we will be stuck to having tensor and par being builtin operators because they re-define brackets...

- Adding ``let" expressions in ``do" blocks. 
  - Note that we changed the grammar to allow a pattern to be present in a phrase like ``get a on channel", so if we write:
        ``get SomeConstructor(a) on channel", 
    This needs to be compiled down to 
        ``let fun a = -> case a of SomeConstructor(a) -> a
          get a on channel".  
    The reason why this was changed was because writing syntax like ``get _ on channel" would be very nice for not caring about a value given (instead of trying to come up with a unique name yourself), and since ``_" is used in patterns already, I thought wouldn't it be nice to be able to completely pattern match against something from a ``get" command.  Moreover, the assembly language does permit intermediate computations like this so it is only natural to allow the the full front end to have this as well.

- Compilation of folds / unfolds (which were deprecated)
  - some discussion on lazily generating map functions and if the map function even exists 

- Lambda lifting needs to be completed
  - Alpha renaming is done already, so this should be fairly straightforward...

- Compilation of pattern matching need to be completed
  - We need to insert the errors for non exhaustive pattern matching for data and codata as well. Type checking does not check if building or pattern matching against a codata record is exhaustive or not.

- Code cleaning:
  - There is no interface to use the system. Currently, it is just a play with in GHCi sort of thing. Write a proper command line interface to the system.
  - Remove all cases of Debug.Trace... right now, it is being used for displaying type equations (useless!)

- Inefficiencies:
  - I am certain that this system does not optimally use unique values (i.e., sometimes it will increment the counter too many times)
  - In the type checker, we can change the Data.Map to use an array (or some other more efficent data structure perhaps a hash map) because we tagged everything with a unique Word id... Although, because of mutability, this would force everything to be in either the ST monad or IO monad.. For now, Data.Map seems to be working fine.


## Wrapping up list...
-- package up to a *tar* and send to Cockett so it is backed up formally...

-- try to get it so that we can tie it up in the future...
-- by WEDNESDAY next week (September 9)
