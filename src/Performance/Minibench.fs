let timeNs = 
    fun (kVal: obj) ->
        let sw = System.Diagnostics.Stopwatch.StartNew()
        sharpurs_apply kVal null |> ignore
        sw.Stop()
        let ticks = sw.ElapsedTicks
        let ns = (float ticks / float System.Diagnostics.Stopwatch.Frequency) * 1e9
        box ns

let gc = 
    fun (dummy: obj) ->
        System.GC.Collect()
        System.GC.WaitForPendingFinalizers()
        null :> obj

let toFixed = 
    fun (nVal: obj) ->
        let n = nVal :?> float
        box (n.ToString("F2", System.Globalization.CultureInfo.InvariantCulture))
