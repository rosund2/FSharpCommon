namespace DataStructures
    
module Graph = 
    open System

    type Id = string
    type Vertice<'V>   = {_VerticeData:'V option; Src:Id; Dst:Id; Id : Id}
    type Node<'O>      = {_NodeData:'O option;Id: Id}

    type Graph<'V,'O>  = {Nodes: Map<Id,Node<'O>>; Vertices: Map<Id,Vertice<'V>>}    
    
    let id () = Guid.NewGuid().ToString()

    let makeGraph<'O,'V > = {Nodes = Map.empty<Id,Node<'O>> ; Vertices = Map.empty<Id,Vertice<'V>>}

    let stringToNode s =
        {Id = s ; _NodeData = None}

    let applyObjectToGraph graph object : Graph<'a,'b> = 
        {graph with Nodes = Map.add object.Id object graph.Nodes}

    let nodesToVertice a b =
        {_VerticeData = None;Src = a.Id ; Dst =  b.Id ; Id = id()}

    let applyVerticeToGraph  graph (vertice:Vertice<'a>) : Graph<'a,'b> = 
        {graph with Vertices = Map.add (vertice.Id) vertice (graph.Vertices)}

    let findObjectById Id graph = 
        if Map.containsKey Id graph.Nodes then
            Map.find Id graph.Nodes
        else            
            failwithf "Could not locate Object with Id: %s in graph" Id

    let stringToNodeToGraph  snode graph = 
        stringToNode snode    
        |> applyObjectToGraph graph

    let connectObjectsById a b graph = 
        Tuple.Create(
            (findObjectById a graph), 
            (findObjectById b graph)
        )
        ||> nodesToVertice  
        |> applyVerticeToGraph graph

    let verticeFilter f g =         
        Map.toList g.Vertices
        |> List.filter (fun (k,v) -> f v) 
        |> List.map (fun (k,v) -> v)
         
    
    let getDestinationsFromObjectId id graph = 
        graph
        |> verticeFilter (fun v -> v.Src = id)
        |> List.map (fun v -> v.Dst)
        

    let resolveObjectById id graph = 
        let o = findObjectById id graph 
        let initIds = getDestinationsFromObjectId o.Id graph
        
        let rec loop vn vo  = 
        
            match vn with 
            |(f :: r) ->                 
                
                let nvlst = 
                    getDestinationsFromObjectId f graph
                    |> Set.ofList
                    |> (fun nlst -> Set.difference nlst (Set.ofList vo))         
                    |> Set.toList
                
                if nvlst.IsEmpty then    
                    loop r (f :: vo )
                else 
                    let r = List.append nvlst r |> Seq.ofList |> Seq.toList
                    loop r (f :: vo)
            | _ -> 
                vo
        
        loop initIds []

    let resolveAll graph = 
        let nodes = 
            List.map (fun (k,v) -> 
                    let oId = v.Id
                    let ores = resolveObjectById oId graph
                    (oId, ores)
                )                 
                (Map.toList graph.Nodes)

        Map.ofList nodes


//open Graph

//let graph = 
//    let pr g = 
//        printfn "%A" g
//        g

//    makeGraph<string,string>
//    |> stringToNodeToGraph "groupA"
//    |> stringToNodeToGraph "groupB" 
//    |> stringToNodeToGraph "groupC" 
//    |> stringToNodeToGraph "groupD" 
//    |> connectObjectsById "groupA" "groupB" 
//    |> connectObjectsById "groupA" "groupC" 
//    |> connectObjectsById "groupC" "groupD" 

//let x = resolveObjectById "groupA" graph
//let y = resolveAll graph






    


