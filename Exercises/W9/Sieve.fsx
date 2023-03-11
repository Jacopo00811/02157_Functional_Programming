let sift a sq = Seq.filter (fun n -> n % a <> 0) sq;

let rec sieve sq =
    Seq.delay (fun () ->
                let p = Seq.head sq
                Seq.append
                    (seq [p])
                    (sieve(sift p (Seq.tail sq))));;


let primes = sieve(Seq.initInfinite (fun n -> n+2));;


let nthPrime n = Seq.item n primes;;

let primesCached = Seq.cache primes;;


let nthPrime' n = Seq.item n primesCached;;


let rec Sieve sq =
    seq { let p = Seq.head sq
          yield p
          yield! Sieve(sift p (Seq.tail sq)) };;


let Sift a sq = seq { for n in sq do
                          if n % a <> 0 then
                                 yield n };;