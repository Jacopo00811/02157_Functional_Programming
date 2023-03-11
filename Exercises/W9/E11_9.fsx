let nat = Seq.initInfinite id;;

let even = seq {for i in nat do
                    if i=0 then yield 0 else 
                    yield -i
                    yield i};;

Seq.toList (Seq.take 15 even);;