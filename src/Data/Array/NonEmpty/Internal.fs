let foldr1Impl =
    fun (f: obj) -> fun (xs: obj) ->
        let arr = xs :?> obj[]
        let mutable acc = arr.[arr.Length - 1]
        for i = arr.Length - 2 downto 0 do
            let step1 = sharpurs_apply f arr.[i]
            acc <- sharpurs_apply step1 acc
        acc

let foldl1Impl =
    fun (f: obj) -> fun (xs: obj) ->
        let arr = xs :?> obj[]
        let mutable acc = arr.[0]
        for i = 1 to arr.Length - 1 do
            let step1 = sharpurs_apply f acc
            acc <- sharpurs_apply step1 arr.[i]
        acc

let traverse1Impl =
    fun (apply: obj) -> fun (mapFn: obj) -> fun (f: obj) -> fun (arrayVal: obj) ->
        let arr = arrayVal :?> obj[]

        let array1 (a: obj) = [| a |] :> obj
        let array2 (a: obj) = 
            (fun (b: obj) -> [| a; b |] :> obj) :> obj
        let array3 (a: obj) = 
            (fun (b: obj) -> 
                (fun (c: obj) -> [| a; b; c |] :> obj) :> obj) :> obj
        
        let concat2 (xsVal: obj) = 
            (fun (ysVal: obj) ->
                let xs = xsVal :?> obj[]
                let ys = ysVal :?> obj[]
                Array.append xs ys :> obj) :> obj

        let rec go bot top =
            let diff = top - bot
            if diff = 1 then
                let mapped = sharpurs_apply mapFn (array1 :> obj)
                sharpurs_apply mapped (sharpurs_apply f arr.[bot])
            elif diff = 2 then
                let mapped = sharpurs_apply mapFn (array2 :> obj)
                let applied = sharpurs_apply apply (sharpurs_apply mapped (sharpurs_apply f arr.[bot]))
                sharpurs_apply applied (sharpurs_apply f arr.[bot + 1])
            elif diff = 3 then
                let mapped = sharpurs_apply mapFn (array3 :> obj)
                let applied1 = sharpurs_apply apply (sharpurs_apply mapped (sharpurs_apply f arr.[bot]))
                let applied2 = sharpurs_apply apply (sharpurs_apply applied1 (sharpurs_apply f arr.[bot + 1]))
                sharpurs_apply applied2 (sharpurs_apply f arr.[bot + 2])
            else
                let pivot = bot + (diff / 4) * 2
                let mapped = sharpurs_apply mapFn (concat2 :> obj)
                let applied = sharpurs_apply apply (sharpurs_apply mapped (go bot pivot))
                sharpurs_apply applied (go pivot top)
        
        go 0 arr.Length
