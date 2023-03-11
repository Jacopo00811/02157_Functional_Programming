// Michael R. Hansen   27-10-2022
module Tests

open System
open Xunit
open Luggage

[<Fact>]
let ``A: empty route`` () =
    Assert.False(inRoute "f" [])

[<Fact>]
let ``B: non-empty route, flight found`` () =
    Assert.True(inRoute "f" [("f","ap")])

[<Fact>]
let ``C: one element route, flight not found`` () =
    Assert.False(inRoute "f" [("f1","ap1")])

[<Fact>]
let ``D: more than one element route, flight found`` () =
    Assert.True(inRoute "f" [("f1","ap1");("f2","ap2");("f","ap")])
