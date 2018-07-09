{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Phantom
  ( mkCar
  , turnOn
  , turnOff
  ) where

-- DataKinds promotes this to a kind and its values to types 'Off and 'On
data CarState
  = Off
  | On

-- KindSignatures lets us use promoted kind as a constraint for our phantom type
newtype Car (s :: CarState) = Car { _name :: String }

-- new cars are always off
mkCar :: String -> Car 'Off
mkCar = Car

-- only cars that are off can be turned on
turnOn :: Car 'Off -> Car 'On
turnOn = Car . _name

-- only cars that are on can be turned off
turnOff :: Car 'On -> Car 'Off
turnOff = Car . _name
