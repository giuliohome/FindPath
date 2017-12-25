// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System

type Port = {p1:int; p2:int}

let readPorts (input:string) : Port [] =
    input.Split('\n')
    |> Array.map (fun l -> l.Split('/'))
    |> Array.map( fun a -> {p1=Int32.Parse(a.[0]);p2=Int32.Parse(a.[1])}  )
      
let weight (p:Port) : int = p.p1 + p.p2

type Path = {endPort:int; w:int; ports:Port[]}
type PathBase = {endPort:int; w:int;}
let getBase (p:Path) : PathBase = {endPort=p.endPort; w=p.w}

let port2Path (p:Port) : (Path * Port) option = 
    match p.p1, p.p2 with
    | 0, _ -> Some ({endPort=p.p2; w=weight p; ports=[|p|]},p)
    | _, 0 -> Some ({endPort=p.p1; w=weight p; ports=[|p|]},p)
    | _ -> None

let combinePathPort (path:Path) (port:Port) : Path [] = 
    [|
    if path.endPort = port.p1 then
        yield { endPort = port.p2; w = path.w + weight port; 
            ports=Array.append path.ports [|port|]}
    if path.endPort = port.p2 then
        yield {endPort = port.p1; w = path.w + weight port; 
            ports=Array.append path.ports [|port|]}
    |] 
    |> Array.distinctBy getBase

let bestPaths (paths:Path[]) : Path[] =
    let dpath = paths |> Array.distinctBy getBase
    let bestWeight = 
        dpath
        |> Array.map (fun p -> p.w )
        |> Array.max
    dpath
    |> Array.filter (fun p -> p.w = bestWeight )

let rec findBestPathWithPorts (path:Path) (ports:Port[]) : Path[] =
    ports
    |> Array.collect( fun port ->
         let alternatives = combinePathPort path port
         match alternatives with
         | [||] -> [|path|]
         | paths -> 
            let rports = 
                ports
                |> Array.except [|port|]
            paths
            |> Array.collect (fun rpath ->
                findBestPathWithPorts rpath rports
            )
            |> bestPaths
    )
    |> bestPaths

let solve (ports:Port[]) : Path[] =
    ports
    |> Array.choose port2Path
    |> Array.collect ( fun (path, port) ->
        let rports = 
            ports
            |> Array.except [|port|]
        findBestPathWithPorts path rports
    )
    |> bestPaths

[<EntryPoint>]
let main argv = 
    let input = @"0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10"
    readPorts input
    |> solve
    |> Array.iter( fun path -> 
        printfn "path weight %i ending with ports %i formed by:" path.w  path.endPort
        path.ports |> Array.iter (printfn "%A")
        )
    Console.ReadKey() |> ignore
    0 // return an integer exit code
