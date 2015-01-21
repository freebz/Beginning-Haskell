--
-- Graph
--

import Data.Graph

timeMachineGraph :: [(String, String, [String])]
timeMachineGraph =
    [("wood","wood",["walls"]),("plastic", "plastic", ["walls", "wheels"])
    ,("aluminum","aluminum",["wheels","door"]),("walls","walls",["done"])
    ,("wheels","wheels",["done"]),("door","door",["done"]),("done","done",[])]

timeMachinePrecedence :: (Graph, Vertex -> (String,String,[String]), String -> Maybe Vertex)
timeMachinePrecedence = graphFromEdges timeMachineGraph
