import Data.Vect

data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type where
  Bicycle : Vehicle Pedal
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  Unicycle : Vehicle Pedal
  Motorcycle : (fuel : Nat) -> Vehicle Petrol
  Tram : (watts : Nat) -> Vehicle Electric
--  Car : (watts : Nat) -> Vehicle Electric

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel (Motorcycle fuel) = Motorcycle 50
refuel Bicycle impossible
refuel Unicycle impossible

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels Unicycle = 1
wheels (Motorcycle fuel) = 2

total vectTake : (m : Nat) -> Vect (m + n) a -> Vect m a
vectTake Z [] = []
vectTake (S k) (x :: xs) = x :: (vectTake k xs)
vectTake Z (x :: xs) = (vectTake Z xs) -- this case is weird

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                                Nothing => Nothing
                                Just idx => Just(index idx xs + index idx ys)
