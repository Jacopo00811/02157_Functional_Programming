let rec collatz n = seq { yield n
                          if n%2=0 then 
                            yield! collatz (n/2)
                          else 
                            yield! collatz (3*n+1)}

//Seq.toList (Seq.take 8 (collatz 4));;


let collatzSequences n = seq { for i in [1..n] do
                               yield collatz i}

//Seq.take 8 (Seq.toList (collatzSequences 5));;

let stoppingTime n = seq { for i in [1..n] do
                           yield Seq.findIndex (fun x -> x=1) (collatz i)}

//Seq.toList (stoppingTime 4);;

