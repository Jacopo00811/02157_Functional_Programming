// randomList n range generates a random list of length n containing integers between 0 and range.
let randomList n range = let rand = let gen = new System.Random()
                                    (fun max -> gen.Next(max))
                         List.init n (fun _ -> rand range);;
