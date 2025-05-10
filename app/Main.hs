module Main where

import Decoder
import Data.Char (isSpace)

main :: IO ()
main = do
    print "Enter JWT:"
    jwtStr <- getLine
    case decodeJwt (trim jwtStr) of
        Left err -> putStrLn err
        Right jwt -> printJwt jwt


printObject :: [(String, String)] -> IO ()
printObject [] = return ()
printObject ((key, value) : rest) = do
    putStrLn $ key ++ " : " ++ value
    printObject rest


printJwt :: Jwt -> IO ()
printJwt (Jwt jwtHeader jwtPayload jwtSignature) = do
    putStrLn "====HEADER===="
    printObject jwtHeader
    putStrLn "====PAYLOAD===="
    printObject jwtPayload
    putStrLn "====SIGNATURE===="
    putStrLn $ "(length " ++ show (length jwtSignature) ++ ")"
    print jwtSignature

trim :: String -> String
trim = takeWhile (not . isSpace) . dropWhile isSpace

