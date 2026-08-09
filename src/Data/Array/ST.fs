open System
open System.Collections.Generic
open System.Linq

let ``new`` =
    (fun () -> System.Collections.Generic.List<obj>() :> obj) :> obj

let peekImpl =
    fun (just: obj) -> fun (nothing: obj) -> fun (iVal: obj) -> fun (xs: obj) ->
        (fun () ->
            let arr = xs :?> System.Collections.Generic.List<obj>
            let i = iVal :?> int
            if i >= 0 && i < arr.Count then
                sharpurs_apply just arr.[i]
            else
                nothing) :> obj

let pokeImpl =
    fun (iVal: obj) -> fun (a: obj) -> fun (xs: obj) ->
        (fun () ->
            let arr = xs :?> System.Collections.Generic.List<obj>
            let i = iVal :?> int
            let ret = i >= 0 && i < arr.Count
            if ret then arr.[i] <- a
            box ret) :> obj

let lengthImpl =
    fun (xs: obj) ->
        (fun () ->
            let arr = xs :?> System.Collections.Generic.List<obj>
            box arr.Count) :> obj

let popImpl =
    fun (just: obj) -> fun (nothing: obj) -> fun (xs: obj) ->
        (fun () ->
            let arr = xs :?> System.Collections.Generic.List<obj>
            if arr.Count > 0 then
                let el = arr.[arr.Count - 1]
                arr.RemoveAt(arr.Count - 1)
                sharpurs_apply just el
            else
                nothing) :> obj

let pushAllImpl =
    fun (asVal: obj) -> fun (xs: obj) ->
        (fun () ->
            let arr = xs :?> System.Collections.Generic.List<obj>
            let as' = asVal :?> obj[]
            arr.AddRange(as')
            box arr.Count) :> obj

let shiftImpl =
    fun (just: obj) -> fun (nothing: obj) -> fun (xs: obj) ->
        (fun () ->
            let arr = xs :?> System.Collections.Generic.List<obj>
            if arr.Count > 0 then
                let el = arr.[0]
                arr.RemoveAt(0)
                sharpurs_apply just el
            else
                nothing) :> obj

let unshiftAllImpl =
    fun (asVal: obj) -> fun (xs: obj) ->
        (fun () ->
            let arr = xs :?> System.Collections.Generic.List<obj>
            let as' = asVal :?> obj[]
            arr.InsertRange(0, as')
            box arr.Count) :> obj

let spliceImpl =
    fun (iVal: obj) -> fun (howManyVal: obj) -> fun (bsVal: obj) -> fun (xs: obj) ->
        (fun () ->
            let arr = xs :?> System.Collections.Generic.List<obj>
            let i = iVal :?> int
            let howMany = howManyVal :?> int
            let bs = bsVal :?> obj[]
            let removed = arr.GetRange(i, howMany)
            arr.RemoveRange(i, howMany)
            arr.InsertRange(i, bs)
            removed.ToArray() :> obj) :> obj

let unsafeFreezeImpl =
    fun (xs: obj) ->
        (fun () ->
            let arr = xs :?> System.Collections.Generic.List<obj>
            arr.ToArray() :> obj) :> obj

let unsafeThawImpl =
    fun (xs: obj) ->
        (fun () ->
            let arr = xs :?> obj[]
            System.Collections.Generic.List<obj>(arr) :> obj) :> obj

let freezeImpl =
    fun (xs: obj) ->
        (fun () ->
            let arr = xs :?> System.Collections.Generic.List<obj>
            arr.ToArray() :> obj) :> obj

let thawImpl =
    fun (xs: obj) ->
        (fun () ->
            let arr = xs :?> obj[]
            System.Collections.Generic.List<obj>(arr) :> obj) :> obj

let cloneImpl =
    fun (xs: obj) ->
        (fun () ->
            let arr = xs :?> System.Collections.Generic.List<obj>
            System.Collections.Generic.List<obj>(arr) :> obj) :> obj

let sortByImpl =
    fun (compare: obj) -> fun (fromOrdering: obj) -> fun (xs: obj) ->
        (fun () ->
            let arr = xs :?> System.Collections.Generic.List<obj>
            if arr.Count < 2 then arr :> obj
            else
                let outArr = arr.ToArray()
                let comparer =
                    { new IComparer<obj> with
                        member _.Compare(a: obj, b: obj) =
                            let step1 = sharpurs_apply compare a
                            let ord = sharpurs_apply step1 b
                            unbox<int> (sharpurs_apply fromOrdering ord) }
                let sorted = outArr.OrderBy((fun x -> x), comparer).ToArray()
                arr.Clear()
                arr.AddRange(sorted)
                arr :> obj) :> obj

let toAssocArrayImpl =
    fun (xs: obj) ->
        (fun () ->
            let arr = xs :?> System.Collections.Generic.List<obj>
            let result = Array.zeroCreate arr.Count
            for i = 0 to arr.Count - 1 do
                let map = Map.empty |> Map.add "value" arr.[i] |> Map.add "index" (box i)
                result.[i] <- box map
            result :> obj) :> obj

let pushImpl =
    fun (a: obj) -> fun (xs: obj) ->
        (fun () ->
            let arr = xs :?> System.Collections.Generic.List<obj>
            arr.Add(a)
            box arr.Count) :> obj
