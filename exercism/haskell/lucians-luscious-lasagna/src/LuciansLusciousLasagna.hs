module LuciansLusciousLasagna (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes) where

expectedMinutesInOven :: Integer
expectedMinutesInOven = 40

preparationTimeInMinutes :: Integer -> Integer
preparationTimeInMinutes = (*2)

elapsedTimeInMinutes :: Integer -> Integer -> Integer
elapsedTimeInMinutes x y = (preparationTimeInMinutes x) + y
