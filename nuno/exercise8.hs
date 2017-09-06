module Lab1 where
import Data.List
import Test.QuickCheck  

data Boy = Matthew | Peter | Jack | Arnold | Carl 
        deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

-- Writing down all of the boy's statements
accuses :: Boy -> Boy -> Bool
accuses Matthew x = (x /= Matthew) && (x /= Carl)
accuses Peter x = (x == Matthew) || (x == Jack)
accuses Jack x = not ( (accuses Matthew x) || (accuses Peter x))
accuses Arnold x = ((accuses Matthew x) || (accuses Peter x)) && not ((accuses Matthew x) && (accuses Peter x))
accuses Carl x = not (accuses Arnold x)

-- Get the list of accusers for a certain boy (aka every boy y in the boys list that accuses x)
accusers :: Boy -> [Boy]
accusers x = [y | y <- boys, accuses y x]

-- If a boy is accused by 3 people, then at least 1 of them are telling the truth because there are only 2 liars.
guilty :: [Boy]
guilty = [x | x <- boys, length (accusers x) == 3]

notGuilty :: [Boy]
notGuilty = [x | x <- boys, length ( accusers x) /= 3]

-- A boy made a honest statement if he accused the guilty boys and didn't acused the non-guilty boys
honest :: [Boy]
honest = nub [x | x <- boys, y <- guilty, z <- notGuilty, accuses x y && not (accuses x z)]

main = print honest