let rec isPrefix xs ys =
    match (xs,ys) with
    | ([],_) -> true
    | (_,[]) -> false
    | (x::xtail,y::ytail) -> x=y && isPrefix xtail ytail;;

    // The empty list is a prefix of any list
    // A non-empty list is not a prefix of the empty list
    // A non-empty list (...) is a prefix of another non-empty list (...) if...