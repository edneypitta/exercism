module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds =  seconds / orbitalPeriodInSeconds planet

orbitalPeriodInSeconds :: Planet -> Float
orbitalPeriodInSeconds Mercury = orbitalPeriodInSeconds Earth * 0.2408467 
orbitalPeriodInSeconds Venus   = orbitalPeriodInSeconds Earth * 0.61519726 
orbitalPeriodInSeconds Earth   = 31557600
orbitalPeriodInSeconds Mars    = orbitalPeriodInSeconds Earth * 1.8808158
orbitalPeriodInSeconds Jupiter = orbitalPeriodInSeconds Earth * 11.862615
orbitalPeriodInSeconds Saturn  = orbitalPeriodInSeconds Earth * 29.447498
orbitalPeriodInSeconds Uranus  = orbitalPeriodInSeconds Earth * 84.016846
orbitalPeriodInSeconds Neptune = orbitalPeriodInSeconds Earth * 164.79132