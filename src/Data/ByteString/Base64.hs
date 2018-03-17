{-# LANGUAGE OverloadedStrings #-}

module Data.ByteString.Base64
  ( encode
  , decode
  , enc
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word
import Data.List (unfoldr)

import Data.ByteString.Internal.Extended
import qualified Data.ByteString.Internal as Internal
import qualified Data.ByteString.Char8 as B8

import Control.Applicative (liftA3)

import Foreign.Ptr (plusPtr, castPtr, Ptr, minusPtr)
import Foreign.ForeignPtr (ForeignPtr(..), castForeignPtr, withForeignPtr)
import Foreign.Storable (peek, poke, peekElemOff)
import System.IO.Unsafe (unsafePerformIO)

clearL :: Word8 -> Int -> Word8
clearL word n = shiftR (shiftL word n) n

peek8 :: Ptr Word8 -> IO Word8
peek8 = peek

poke8 :: Ptr Word8 -> Word8 -> IO ()
poke8 = poke

peek8_32 :: Ptr Word8 -> IO Word32
peek8_32 = fmap fromIntegral . peek

poke8x4 :: Ptr Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> IO ()
poke8x4 dp w1 w2 w3 w4 = do
  poke dp w1
  poke (dp `plusPtr` 1) w2
  poke (dp `plusPtr` 2) w3
  poke (dp `plusPtr` 3) w4

peek8x3 :: Ptr Word8 -> IO Word32
peek8x3 sp = do
  i <- peek8_32 sp
  j <- peek8_32 (sp `plusPtr` 1)
  k <- peek8_32 (sp `plusPtr` 2)
  return (i `shiftL` 16 .|. j `shiftL` 8 .|. k)

nthWord6 :: Word32 -> Int -> Word8
nthWord6 word32 n = fromIntegral $ (word32 `shiftR` (18 - (n * 6))) .&. 0x3f

peek8_fn :: Ptr Word8 -> Int -> (Word8 -> Word8) -> IO Word8
peek8_fn ptr off f = f <$> peek (ptr `plusPtr` off)

encode :: Enc -> B8.ByteString -> B8.ByteString
encode enc (Internal.PS sfp soff slen) =
  unsafePerformIO $ do
    let dlen = ((slen + 2) `div` 3) * 4
    dfp <- Internal.mallocByteString dlen
    withForeignPtr sfp $ \sptr ->
      withForeignPtr dfp $ \dptr -> 
        fill (castPtr dptr) (castPtr sptr) (sptr `plusPtr` (soff + slen))
    return $ Internal.PS dfp 0 dlen

fill :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
fill dp sp sEnd
  | sp `plusPtr` 2 >= sEnd = complete (castPtr dp) (castPtr sp) sEnd
  | otherwise = do
    -- Produce a Word32 where the 24 right-most bytes are the contents
    -- of (sp..sp+2)
    w <- liftA3 pack8x3 (peek sp) (peek $ sp `plusPtr` 1) (peek $ sp `plusPtr` 2)
    -- Unpack the Word32 6 bytes at a time, encode them, and place into `dp`
    poke8s dp 
      $ map (encodeWord enc) 
      $ unpack6x4 w
    fill (dp `plusPtr` 4) (sp `plusPtr` 3) sEnd

complete :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
complete dp sp sEnd
  | sp == sEnd = return ()
  | otherwise = do
    let rem = if sp `plusPtr` 2 == sEnd then 2 else 1
    w <- liftA3 pack8x3 
      (peek sp) 
      (if rem == 2 then peek $ sp `plusPtr` 1 else return 0) 
      (return 0)
    let bytes = map (encodeWord enc) $ unpack6x4 w
    poke8 dp               (bytes !! 0)
    poke8 (dp `plusPtr` 1) (bytes !! 1)
    poke8 (dp `plusPtr` 2) (if rem == 2 then bytes !! 2 else 0x3d)
    poke8 (dp `plusPtr` 3) 0x3d

decode :: Enc -> B8.ByteString -> Either String B8.ByteString
decode enc (Internal.PS sfp soff slen)
  | drem /=0 = Left "Invalid padding"
  | dlen <= 0 = Right B8.empty
  | otherwise = unsafePerformIO $ do
    dfp <- Internal.mallocByteString dlen
    withForeignPtr sfp $ \sptr -> do
      let
        sEnd = sptr `plusPtr` (soff + slen)
        finish n = return $ Right $ if n > 0 then Internal.PS dfp 0 n else B8.empty

        getWords :: Int -> Ptr Word8 -> IO [Word8]
        getWords n sp = (traverse $ peek . (sp `plusPtr`)) [0..n]

        decodeWords :: IO [Word8] -> IO (Either String [Word8])
        decodeWords = fmap $ traverse (decodeWord enc)

        isEquals words n = words !! n == 0x3d

        fill :: Ptr Word8 -> Ptr Word8 -> Int -> IO (Either String B8.ByteString)
        fill dp sp n
          | sp >= sEnd = finish n
          | otherwise = do
            words <- getWords 4 sp
            if isEquals words 0 || isEquals words 1
              then return $ Left "wat"
            else if isEquals words 2 && not (isEquals words 3)
              then return $ Left "WAT"
              else do
                decoded <- decodeWords $ return $
                  [ words !! 0
                  , words !! 1
                  ]
                  ++ if isEquals words 2 then [] else [words !! 2]
                  ++ if isEquals words 3 then [] else [words !! 3]
                case decoded of
                  Left err -> return $ Left err
                  Right got -> do
                    let g = got ++ replicate (4 - length got) 0
                        bytes = pack6x4 (reverse g)
                    poke8 dp               (unpack8 2 bytes)
                    if isEquals words 2
                      then finish (n+1)
                      else do
                        poke8 (dp `plusPtr` 1) (unpack8 1 bytes)
                        if isEquals words 3
                          then finish (n+2)
                          else do
                            poke8 (dp `plusPtr` 2) (unpack8 0 bytes)
                            fill (dp `plusPtr` 3) (sp `plusPtr` 4) (n+3)

      withForeignPtr dfp $ \dptr ->
        fill dptr sptr 0
  where
    (di, drem) = slen `divMod` 4
    dlen = di * 3


-- decode' :: Enc -> B8.ByteString -> Either String B8.ByteString
-- decode' enc (Internal.PS sfp soff slen)
--   | drem /= 0 = Left "Invalid padding"
--   | dlen <= 0 = Right B8.empty
--   | otherwise = unsafePerformIO $ do
--     dfp <- Internal.mallocByteString dlen
--     withForeignPtr sfp $ \sptr -> do
--       let
--         finish dbytes = return . Right $ if dbytes > 0 then Internal.PS dfp 0 dbytes else B8.empty
--         sEnd = sptr `plusPtr` (soff + slen)
--         look p = (decodeWord enc . fromIntegral) <$> peek8 p
--         fill dp sp n
--           | sp >= sEnd = finish n
--           | otherwise = do
--           a <- look sp
--           b <- look (sp `plusPtr` 1)
--           c <- look (sp `plusPtr` 2)
--           d <- look (sp `plusPtr` 3)
--   where
--     (di, drem) = slen `divMod` 4
--     dlen = di * 3

base64 :: B8.ByteString
base64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

enc :: Enc
enc = mkEnc base64 "="


-- Pack a Word32 with a 6-bits at offset `off`. Offset is counted from the right
-- so `pack6 word 0 byte` packs the right-most 6 bits.
pack6 :: Int -> Word8 -> Word32 -> Word32
pack6 off word buf = buf .|. (fromIntegral word `shiftL` (off * 6))

-- Unpack 6 bits from a Word32 at offset `off`. Offset is counted form the right
-- so `unpack6 word 0` unpacks the 6 right-most bits into a Word8.
unpack6 :: Int -> Word32 -> Word8
unpack6 off buf = fromIntegral $ buf `shiftR` (6 * off) .&. 0x3f


pack8x3 :: Word8 -> Word8 -> Word8 -> Word32
pack8x3 b3 b2 b1 = foldr (uncurry pack8) 0 (zip [0..2] [b1, b2, b3])

pack6x4 :: [Word8] -> Word32
pack6x4 bs = foldr (uncurry pack6) 0 (zip [0..3] bs)

unpack8x3 :: Word32 -> [Word8]
-- unpack8x3 = unpack unpack8 3
unpack8x3 w = [ unpack8 2 w, unpack8 1 w, unpack8 0 w ]

-- FIXME: byte ordering not correct
-- unpack6x4 :: Word32 -> [Word8]
-- unpack6x4 = unpack unpack6 4
unpack6x4 :: Word32 -> [Word8]
unpack6x4 w =
  [ unpack6 3 w, unpack6 2 w, unpack6  1 w, unpack6 0 w]

encode' :: Enc -> B8.ByteString -> B8.ByteString
encode' enc src@(Internal.PS sfp soff slen) =
  unsafePerformIO $ byChunk 3 (((slen + 2) `div` 3) * 4) onchunk onend src

  where
    onchunk sp dp = do
      w <- liftA3 pack8x3 (peek sp) (peek $ sp `plusPtr` 1) (peek $ sp `plusPtr` 2)
      poke8s dp 
        $ map (encodeWord enc) 
        $ unpack6x4 w
      return 4

    onend sp dp 0 = return ()
    onend sp dp rem = do
      print rem
      w <- liftA3 pack8x3 
        (peek sp) 
        (if rem == 2 then peek $ sp `plusPtr` 1 else return 0) 
        (return 0)
      let bytes = map (encodeWord enc) $ unpack6x4 w
      poke8 dp               (bytes !! 0)
      poke8 (dp `plusPtr` 1) (bytes !! 1)
      poke8 (dp `plusPtr` 2) (if rem == 2 then bytes !! 2 else 0x3d)
      poke8 (dp `plusPtr` 3) 0x3d
