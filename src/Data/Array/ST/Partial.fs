let peekImpl = 
    fun (iVal: obj) -> fun (xs: obj) ->
        let i = iVal :?> int
        let arr = xs :?> System.Collections.Generic.List<obj>
        arr.[i]

let pokeImpl = 
    fun (iVal: obj) -> fun (a: obj) -> fun (xs: obj) ->
        let i = iVal :?> int
        let arr = xs :?> System.Collections.Generic.List<obj>
        arr.[i] <- a
        null :> obj
