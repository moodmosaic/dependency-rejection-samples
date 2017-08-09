module Ploeh.Samples.MaîtreDTests

open Ploeh.Samples.MaîtreD
open Hedgehog
open Xunit
open System
open Swensen.Unquote

module Tuple2 =
    let curry f x y = f (x, y)

module Gen =
    let reservation =
        gen {
            let! bookingDate = Gen.dateTime |> Gen.map DateTimeOffset
            let! positiveQty = Gen.int (Range.linear 1 100)
            let! trueOrFalse = Gen.bool

            return { Date = bookingDate
                     Quantity = positiveQty
                     IsAccepted = trueOrFalse }
        }

[<Fact>]
let ``tryAccept behaves correctly when it can accept`` () =
    property {
        let! reservation    = Gen.reservation
        let! reservations   = Gen.list (Range.linear 0 100) Gen.reservation
        let! excessCapacity = Gen.int  (Range.linear 0 100)
        let  capacity       = excessCapacity
                              + (reservations |> List.sumBy (fun x -> x.Quantity))
                              + reservation.Quantity

        let actual = tryAccept capacity reservations reservation

        return Some { reservation with IsAccepted = true } = actual
    }

[<Fact>]
let ``tryAccept behaves correctly when it can't accept`` () =
    property {
        let! reservation     = Gen.reservation
        let! reservations    = Gen.list (Range.linear 0 100) Gen.reservation
        let! lackingCapacity = Gen.int  (Range.linear 1 100)
        let  capacity        = (reservations |> List.sumBy (fun x -> x.Quantity))
                               - lackingCapacity

        let actual = tryAccept capacity reservations reservation

        return None = actual
    }

