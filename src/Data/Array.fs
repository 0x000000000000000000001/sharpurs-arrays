open System
open System.Collections.Generic
open System.Linq

let rangeImpl =
    fun (startVal: obj) -> fun (endVal: obj) ->
        let start = startVal :?> int
        let endV = endVal :?> int
        let step = if start > endV then -1 else 1
        let size = (endV - start) * step + 1
        let result = Array.zeroCreate size
        let mutable i = start
        let mutable n = 0
        while i <> endV do
            result.[n] <- box i
            n <- n + 1
            i <- i + step
        result.[n] <- box i
        result :> obj

let replicateImpl =
    fun (countVal: obj) -> fun (value: obj) ->
        let count = countVal :?> int
        if count < 1 then Array.empty<obj> :> obj
        else
            let result = Array.zeroCreate count
            for i = 0 to count - 1 do
                result.[i] <- value
            result :> obj

let length =
    fun (xs: obj) ->
        let arr = xs :?> obj[]
        arr.Length :> obj

let unconsImpl =
    fun (empty: obj) -> fun (next: obj) -> fun (xs: obj) ->
        let arr = xs :?> obj[]
        if arr.Length = 0 then
            sharpurs_apply empty null
        else
            let head = arr.[0]
            let tail = Array.zeroCreate (arr.Length - 1)
            Array.Copy(arr, 1, tail, 0, arr.Length - 1)
            sharpurs_apply (sharpurs_apply next head) (tail :> obj)

let indexImpl =
    fun (just: obj) -> fun (nothing: obj) -> fun (xs: obj) -> fun (iVal: obj) ->
        let arr = xs :?> obj[]
        let i = iVal :?> int
        if i < 0 || i >= arr.Length then nothing
        else
            sharpurs_apply just arr.[i]

let _updateAt =
    fun (just: obj) -> fun (nothing: obj) -> fun (iVal: obj) -> fun (a: obj) -> fun (xs: obj) ->
        let arr = xs :?> obj[]
        let i = iVal :?> int
        if i < 0 || i >= arr.Length then nothing
        else
            let l1 = Array.zeroCreate arr.Length
            Array.Copy(arr, 0, l1, 0, arr.Length)
            l1.[i] <- a
            sharpurs_apply just (l1 :> obj)

let _insertAt =
    fun (just: obj) -> fun (nothing: obj) -> fun (iVal: obj) -> fun (a: obj) -> fun (xs: obj) ->
        let arr = xs :?> obj[]
        let i = iVal :?> int
        if i < 0 || i > arr.Length then nothing
        else
            let l1 = Array.zeroCreate (arr.Length + 1)
            Array.Copy(arr, 0, l1, 0, i)
            l1.[i] <- a
            Array.Copy(arr, i, l1, i + 1, arr.Length - i)
            sharpurs_apply just (l1 :> obj)

let _deleteAt =
    fun (just: obj) -> fun (nothing: obj) -> fun (iVal: obj) -> fun (xs: obj) ->
        let arr = xs :?> obj[]
        let i = iVal :?> int
        if i < 0 || i >= arr.Length then nothing
        else
            let l1 = Array.zeroCreate (arr.Length - 1)
            Array.Copy(arr, 0, l1, 0, i)
            Array.Copy(arr, i + 1, l1, i, arr.Length - i - 1)
            sharpurs_apply just (l1 :> obj)

let reverse =
    fun (xs: obj) ->
        let arr = xs :?> obj[]
        let l1 = Array.zeroCreate arr.Length
        for i = 0 to arr.Length - 1 do
            l1.[i] <- arr.[arr.Length - 1 - i]
        l1 :> obj

let concat =
    fun (xss: obj) ->
        let arrs = xss :?> obj[]
        let mutable totalLength = 0
        for xs in arrs do
            totalLength <- totalLength + (xs :?> obj[]).Length
        let result = Array.zeroCreate totalLength
        let mutable current = 0
        for xs in arrs do
            let xsArr = xs :?> obj[]
            Array.Copy(xsArr, 0, result, current, xsArr.Length)
            current <- current + xsArr.Length
        result :> obj

let filterImpl =
    fun (f: obj) -> fun (xs: obj) ->
        let arr = xs :?> obj[]
        let res = ResizeArray<obj>()
        for x in arr do
            if unbox<bool> (sharpurs_apply f x) then res.Add(x)
        res.ToArray() :> obj

let sliceImpl =
    fun (sVal: obj) -> fun (eVal: obj) -> fun (lVal: obj) ->
        let mutable s = sVal :?> int
        let mutable e = eVal :?> int
        let l = lVal :?> obj[]
        if s < 0 then s <- l.Length + s
        if e < 0 then e <- l.Length + e
        if s < 0 then s <- 0
        if e > l.Length then e <- l.Length
        if s > e then s <- e
        
        let res = Array.zeroCreate (e - s)
        Array.Copy(l, s, res, 0, e - s)
        res :> obj

let zipWithImpl =
    fun (f: obj) -> fun (xs: obj) -> fun (ys: obj) ->
        let arrX = xs :?> obj[]
        let arrY = ys :?> obj[]
        let length = Math.Min(arrX.Length, arrY.Length)
        let result = Array.zeroCreate length
        for i = 0 to length - 1 do
            let step1 = sharpurs_apply f arrX.[i]
            result.[i] <- sharpurs_apply step1 arrY.[i]
        result :> obj

let unsafeIndexImpl =
    fun (xs: obj) -> fun (n: obj) ->
        let arr = xs :?> obj[]
        let i = n :?> int
        arr.[i]

let sortByImpl =
    fun (compare: obj) -> fun (fromOrdering: obj) -> fun (xs: obj) ->
        let arr = xs :?> obj[]
        if arr.Length < 2 then arr :> obj
        else
            let comparer =
                { new IComparer<obj> with
                    member _.Compare(a: obj, b: obj) =
                        let step1 = sharpurs_apply compare a
                        let ord = sharpurs_apply step1 b
                        unbox<int> (sharpurs_apply fromOrdering ord) }
            let sorted = arr.OrderBy((fun x -> x), comparer).ToArray()
            sorted :> obj

let scanrImpl =
    fun (f: obj) -> fun (b: obj) -> fun (xs: obj) ->
        let arr = xs :?> obj[]
        let outArr = Array.zeroCreate arr.Length
        let mutable acc = b
        for i = arr.Length - 1 downto 0 do
            let step1 = sharpurs_apply f arr.[i]
            acc <- sharpurs_apply step1 acc
            outArr.[i] <- acc
        outArr :> obj

let scanlImpl =
    fun (f: obj) -> fun (b: obj) -> fun (xs: obj) ->
        let arr = xs :?> obj[]
        let outArr = Array.zeroCreate arr.Length
        let mutable acc = b
        for i = 0 to arr.Length - 1 do
            let step1 = sharpurs_apply f acc
            acc <- sharpurs_apply step1 arr.[i]
            outArr.[i] <- acc
        outArr :> obj

let partitionImpl =
    fun (f: obj) -> fun (xs: obj) ->
        let arr = xs :?> obj[]
        let yes = ResizeArray<obj>()
        let no = ResizeArray<obj>()
        for x in arr do
            if unbox<bool> (sharpurs_apply f x) then yes.Add(x)
            else no.Add(x)
        let res = Map.empty<string, obj>
        let res = res.Add("yes", yes.ToArray() :> obj)
        let res = res.Add("no", no.ToArray() :> obj)
        box res

type private ConsList =
    | Cons of obj * ConsList
    | EmptyList

let fromFoldableImpl =
    fun (foldr: obj) -> fun (xsVal: obj) ->
        let cons = fun (head: obj) -> box (fun (tail: obj) -> box (Cons(head, unbox<ConsList> tail)))
        let listObj = Sharpurs_Prelude.sharpurs_apply (Sharpurs_Prelude.sharpurs_apply (Sharpurs_Prelude.sharpurs_apply foldr (box cons)) (box EmptyList)) xsVal
        let list = unbox<ConsList> listObj
        
        let rec countElements (l: ConsList) acc =
            match l with
            | EmptyList -> acc
            | Cons(_, tail) -> countElements tail (acc + 1)
        
        let size = countElements list 0
        let result = Array.zeroCreate size
        
        let rec fillArray (l: ConsList) i =
            match l with
            | EmptyList -> ()
            | Cons(head, tail) -> 
                result.[i] <- head
                fillArray tail (i + 1)
                
        fillArray list 0
        result :> obj

let findMapImpl =
    fun (nothing: obj) -> fun (isJust: obj) -> fun (f: obj) -> fun (xs: obj) ->
        let arr = xs :?> obj[]
        let mutable result = nothing
        let mutable i = 0
        while i < arr.Length && obj.ReferenceEquals(result, nothing) do
            let res = sharpurs_apply f arr.[i]
            if unbox<bool> (sharpurs_apply isJust res) then result <- res
            i <- i + 1
        result

let findLastIndexImpl =
    fun (just: obj) -> fun (nothing: obj) -> fun (f: obj) -> fun (xs: obj) ->
        let arr = xs :?> obj[]
        let mutable result = nothing
        let mutable i = arr.Length - 1
        while i >= 0 && obj.ReferenceEquals(result, nothing) do
            if unbox<bool> (sharpurs_apply f arr.[i]) then
                result <- sharpurs_apply just (box i)
            i <- i - 1
        result

let findIndexImpl =
    fun (just: obj) -> fun (nothing: obj) -> fun (f: obj) -> fun (xs: obj) ->
        let arr = xs :?> obj[]
        let mutable result = nothing
        let mutable i = 0
        while i < arr.Length && obj.ReferenceEquals(result, nothing) do
            if unbox<bool> (sharpurs_apply f arr.[i]) then
                result <- sharpurs_apply just (box i)
            i <- i + 1
        result

let anyImpl =
    fun (p: obj) -> fun (xs: obj) ->
        let arr = xs :?> obj[]
        let mutable result = false
        let mutable i = 0
        while i < arr.Length && not result do
            if unbox<bool> (sharpurs_apply p arr.[i]) then result <- true
            i <- i + 1
        box result

let allImpl =
    fun (p: obj) -> fun (xs: obj) ->
        let arr = xs :?> obj[]
        let mutable result = true
        let mutable i = 0
        while i < arr.Length && result do
            if not (unbox<bool> (sharpurs_apply p arr.[i])) then result <- false
            i <- i + 1
        box result
