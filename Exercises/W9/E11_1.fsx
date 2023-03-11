let even = Seq.initInfinite (fun x -> 2*x+1);;
Seq.toList (Seq.take 15 even);;