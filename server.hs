-- Urban Heat Island Recursive Simulation

-- Function to calculate next year's temperature
nextTemp :: Double -> Double -> Double -> Double -> Double
nextTemp current urban pop climate =
    current + urban + pop + climate

-- Recursive simulation
simulateUHI :: Int -> Double -> Double -> Double -> Double -> [Double]
simulateUHI 0 temp _ _ _ = [temp]
simulateUHI years temp urban pop climate =
    temp : simulateUHI (years - 1) next urban pop climate
    where
        next = nextTemp temp urban pop climate

main :: IO ()
main = do

    putStrLn "Urban Heat Island Simulation"

    putStrLn "Enter current temperature:"
    t <- readLn

    putStrLn "Enter urbanization impact:"
    u <- readLn

    putStrLn "Enter population impact:"
    p <- readLn

    putStrLn "Enter climate impact:"
    c <- readLn

    putStrLn "Enter number of years:"
    y <- readLn

    let result = simulateUHI y t u p c

    putStrLn "Predicted temperatures:"
    print result
