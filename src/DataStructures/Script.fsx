
#load "Graph.fs"

open DataStructures.Graph

type Folder = {Name:string}
type File   = {Name:string}

type FNode = 
    | FolderNode of Folder
    | FileNode of File

type Entry =  {AddedDate:string}


let fnodeToNode nodeId node =     
    {Id = nodeId ; _NodeData = Some node}
    
let fnodeToToGraph nodeId node graph = 
    fnodeToNode nodeId node 
    |> applyObjectToGraph graph

let nodeGraph = 
    makeGraph<FNode,Entry>
    |> fnodeToToGraph "jatta" (FolderNode {Name = "Jatta"}) 
    |> fnodeToToGraph "jatta2" (FileNode {Name = "Jatta2"})
    |> connectObjectsById "jatta" "jatta2"

let strGraph = 
    makeGraph<string,string>
    |> stringToNodeToGraph "groupA"
    |> stringToNodeToGraph "groupB" 
    |> stringToNodeToGraph "groupC" 
    |> stringToNodeToGraph "groupD" 
    |> connectObjectsById "groupA" "groupB" 
    |> connectObjectsById "groupA" "groupC" 
    |> connectObjectsById "groupC" "groupD" 