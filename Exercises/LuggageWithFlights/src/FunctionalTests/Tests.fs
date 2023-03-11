// Michael R. Hansen  27-10-2022

module Tests

open System
open Xunit
open Luggage

[<Fact>]
let ``A: empty route`` () =
    Assert.True(withFlight "f" [] = [])

[<Fact>]
let ``B1: one-elem. cat., flight first in route`` () =
    Assert.True(withFlight "f" [("lid", [("f","ap")])] = ["lid"])

[<Fact>]
let ``B2: one-elem. cat., flight later in route`` () =
    Assert.True(withFlight "f" [("lid",[("f1","ap1");("f","ap")])] = ["lid"])

[<Fact>]
let ``B3: one-elem. cat., flight not in route`` () =
    Assert.True(withFlight "f" [("lid",[("f1","ap1");("f2","ap")])] = [])


let catC1 = [("lid1", [("f1","a1")]);
             ("lid2", [("f2","a2"); ("f3","a3")]) ]
[<Fact>]
let ``C1: one+-elem. cat., flight not in any route`` () =
    Assert.True(withFlight "f" catC1 = [])


let catC2a = [("lid1", [("f1","a1")]);
              ("lid2", [("f","a"); ("f2","a2")]) ]
[<Fact>]
let ``C2a: one+-elem. cat., flight appears first in a route`` () =
    Assert.True(withFlight "f" catC2a = ["lid2"])


let catC2b = [("lid1", [("f1","a1"); ("f","a"); ("f2","a2")]);
              ("lid2", [("f3","a3")])]
[<Fact>]
let ``C1b: one+-elem. cat., flight appears inside a route`` () =
    Assert.True(withFlight "f" catC2b = ["lid1"])


let catC2c = [("lid1", [("f1","a1")]);
              ("lid2", [("f2","a2"); ("f","a")]) ]
[<Fact>]
let ``C2c: one+-elem. cat., flight appears at the end of a route`` () =
    Assert.True(withFlight "f" catC2c = ["lid2"])

let catC3 = [("lid1", [("f","a1")]);
             ("lid2", [("f2","a2"); ("f","a")]) ]
[<Fact>]
let ``C3: one+-elem. cat., flight appears in several routes`` () =
    Assert.True(Set.ofList(withFlight "f" catC3) = set["lid1"; "lid2"])