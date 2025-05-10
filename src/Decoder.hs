module Decoder where

import Data.Maybe(isJust, mapMaybe)
import Data.Char(ord, chr)
import GHC.Unicode
import Data.Bits


characterIndex :: Char -> Maybe Int
characterIndex char
    | isAsciiUpper char = Just (ord char - ord 'A')
    | isAsciiLower char = Just (26 + ord char - ord 'a')
    | isDigit char = Just (52 + ord char - ord '0')
    | char == '-' = Just 62
    | char == '_' = Just 63
    | otherwise = Nothing


charsToValues :: String -> Either String [Int]
charsToValues str
    | validated = Right $ mapMaybe characterIndex str
    | otherwise = Left "Invalid jwt characters"
    where
        validated = all (\c -> isJust (characterIndex c) || (c == '=')) str


valuesToBits :: [Int] -> [[Int]]
valuesToBits = map valueToBits
    where
        valueToBits v = reverse $ foldl (\acc x -> acc ++ [v `shiftR` x .&. 1] ) [] [0..5]

groupIntoByte :: [Int] -> [[Int]]
groupIntoByte [] = []
groupIntoByte xs = filter ((8 ==) . length) $ take 8 xs : groupIntoByte (drop 8 xs)

decodeBase64 :: String -> Either String String
decodeBase64 jwt = decode . groupIntoByte . concat . valuesToBits <$> charsToValues jwt
    where
        decode = foldl (\acc bits -> acc ++ [bitsToChar bits]) ""
        bitsToChar bits = chr $ foldl (\acc x -> acc * 2 + x) 0 bits



splitString :: Char -> String -> [String]
splitString sep = foldr splitHelper [""]
    where
        splitHelper c (x : xs)
            | c == sep = "" : (x : xs)
            | otherwise = (c : x) : xs


data Jwt = Jwt {
        jwtHeader :: [(String, String)],
        jwtPayload :: [(String, String)],
        jwtSignature :: String
    } deriving(Show)


decodeJwtObject :: String -> [(String, String)]
decodeJwtObject str = map findKeyValuePair $ splitString ',' $ filter isValidChar str
    where
        isValidChar c = isPrint c && c /= '{'  && c /= '}' && c /= '"' 
        findKeyValuePair "" = ("", "")
        findKeyValuePair (':' : xs) = ("", xs)
        findKeyValuePair (x : xs) = (x :key, value)
            where
                (key, value) = findKeyValuePair xs


decodeJwt :: String -> Either String Jwt
decodeJwt jwtString
    | partsLen /= 3 = Left "Jwt has incorrect number of parts"
    | otherwise = do
        headerRaw <- decodeBase64 $ head parts
        let header = decodeJwtObject headerRaw
        bodyRaw <- decodeBase64 $ parts !! 1
        signature <- decodeBase64 $ parts !! 2
        let body = decodeJwtObject bodyRaw
        Right Jwt { jwtHeader = header, jwtPayload = body, jwtSignature = signature}
    where
        splitJwtString = splitString '.'
        parts = splitJwtString jwtString
        partsLen = length parts
