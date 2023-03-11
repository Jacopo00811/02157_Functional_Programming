type ListTree<'a> = Node of 'a * (ListTree<'a> list)


let rec depthFirstList = function
    | [] -> []
    | Node(n,ts)::trest -> n::depthFirstList(ts @ trest)