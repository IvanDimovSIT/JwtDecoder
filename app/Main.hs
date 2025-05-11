module Main where

import Decoder
import Data.Char (isSpace)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format
import Text.Read (readMaybe)


main :: IO ()
main = do
    putStrLn "Enter JWT:"
    jwtStr <- getLine
    case decodeJwt (trim jwtStr) of
        Left err -> putStrLn err
        Right jwt -> printJwt jwt


printObject :: [(String, String)] -> IO ()
printObject [] = return ()
printObject ((key, value) : rest) = do
    putStrLn $ key ++ " : " ++ checkAndDisplayAsDate key value
    printObject rest
    where
        isDateKey key = key `elem` ["iat", "exp", "nbf", "auth_time", "updated_at", "rat"]
        convertToDate str = posixSecondsToUTCTime . fromIntegral <$> (readMaybe str :: Maybe Int)
        checkAndDisplayAsDate key value
            | isDateKey key =
                value ++ maybe "" (formatTime defaultTimeLocale " (UTC %Y-%m-%d %H:%M:%S)") (convertToDate value)
            | otherwise = value


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

