let ``undefined`` = null :> obj
let defined = fun (x: obj) -> x

let eqUndefinedOrImpl = 
    fun (eq: obj) -> fun (a: obj) -> fun (b: obj) ->
        if isNull a && isNull b then box true
        else if isNull a || isNull b then box false
        else
            sharpurs_apply (sharpurs_apply eq a) b

let compareUndefinedOrImpl = 
    fun (lt: obj) -> fun (eq: obj) -> fun (gt: obj) -> fun (compare: obj) -> fun (a: obj) -> fun (b: obj) ->
        if isNull a && isNull b then eq
        elif isNull a then lt
        elif isNull b then gt
        else
            sharpurs_apply (sharpurs_apply compare a) b
