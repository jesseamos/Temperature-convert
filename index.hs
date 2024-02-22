tempConvertLogic :: Double->String -> String-> Double
tempConvertLogic tempVal inputScale outPutScale
  | inputScale == "C" && outPutScale == "F" = (tempVal * 9/5) + 32
  | inputScale == "F" && outPutScale=="C" = (tempVal - 32 )* 5/9
  | inputScale == "C" && outPutScale == "K" = tempVal + 273.15
  | inputScale == "K" && outPutScale == "C" = tempVal - 273.15
  | inputScale == "F" && outPutScale == "K" = (tempVal + 459.67)*5/9
  | inputScale == "K" && outPutScale == "F" = tempVal * 9/5 - 459.67
  | otherwise = error "Invalid Conversion"



main :: IO()
main =  do
    putStrLn "Please Enter Input Value"
    tempValStr <- getLine
    putStrLn "Specify the Input Scale (e.g., Celsius, Fahrenheit, Kelvin)."
    inputScale <- getLine
    putStrLn "specify the output temperature scale (e.g., Celsius, Fahrenheit, Kelvin"
    outPutScale <- getLine
    let tempVal = read tempValStr::Double
    let convertedTemp = tempConvertLogic tempVal inputScale outPutScale
    putStrLn $ show tempVal ++ " being converted from " ++ inputScale ++ " to " ++ outPutScale ++ " is: " ++ show convertedTemp