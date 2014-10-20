let rec transpose = function
| []          -> []
| []::tail    -> transpose tail
| (x::t1)::t2 ->
    let new_head = (x::(List.map List.hd t2)) 
    and new_tail =  (transpose (t1 :: (List.map List.tl t2) ))
    in 
    new_head @ new_tail
;;


