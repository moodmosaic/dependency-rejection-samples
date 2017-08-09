module MaitreDTests
    ( tryAcceptBehavesCorrectlyWhenItCanAccept
    , tryAcceptBehavesCorrectlyWhenItCanNotAccept
    ) where

import           Control.Monad         (liftM2)
import           Data.Functor.Identity (Identity (..))
import           Data.Maybe            (isNothing)
import           Data.Time             (LocalTime (..), ZonedTime (..),
                                        midnight, utc)
import           Data.Time.Calendar    (fromGregorian, gregorianMonthLength)
import           MaitreD

import           Hedgehog
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range

genZonedTime :: GenT Identity ZonedTime
genZonedTime = do
    y <- Gen.int (Range.constant 1 9999)
    m <- Gen.int (Range.constant 1 12)
    d <- Gen.int (Range.constant 1 (gregorianMonthLength (toInteger y) m))
    return $ ZonedTime (LocalTime (fromGregorian (toInteger y) m d) midnight) utc

genReservation :: GenT Identity Reservation
genReservation = do
  bookingDate <- genZonedTime
  positiveQty <- Gen.int (Range.linear 1 100)
  trueOrFalse <- Gen.bool
  return Reservation
    { date       = bookingDate
    , quantity   = positiveQty
    , isAccepted = trueOrFalse }

sumBy :: Num a => (b -> a) -> [b] -> a
sumBy x xs = sum $ map x xs

tryAcceptBehavesCorrectlyWhenItCanAccept :: Property
tryAcceptBehavesCorrectlyWhenItCanAccept =
  property $ do
    reservation    <- forAll genReservation
    reservations   <- forAll $ Gen.list (Range.linear 0 100) genReservation
    excessCapacity <- forAll $ Gen.int  (Range.linear 0 100)
    let capacity =
          excessCapacity
          + sumBy quantity reservations
          + quantity reservation

        actual = tryAccept capacity reservations reservation

    Just (reservation { isAccepted = True }) === actual

tryAcceptBehavesCorrectlyWhenItCanNotAccept :: Property
tryAcceptBehavesCorrectlyWhenItCanNotAccept =
  property $ do
    reservation     <- forAll genReservation
    reservations    <- forAll $ Gen.list (Range.linear 0 100) genReservation
    lackingCapacity <- forAll $ Gen.int  (Range.linear 1 100)
    let capacity = sumBy quantity reservations - lackingCapacity

        actual = tryAccept capacity reservations reservation

    Nothing === actual
