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
ageOn planet seconds = seconds / (secondsInEarthYear * conversionFactor planet)

secondsInEarthYear :: Float
secondsInEarthYear = 31557600

conversionFactor :: Planet -> Float
conversionFactor Mercury = 0.2408467 
conversionFactor Venus   = 0.61519726 
conversionFactor Earth   = 1
conversionFactor Mars    = 1.8808158
conversionFactor Jupiter = 11.862615
conversionFactor Saturn  = 29.447498
conversionFactor Uranus  = 84.016846
conversionFactor Neptune = 164.79132