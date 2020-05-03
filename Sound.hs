module Sound (textToMorse) where

import Data.WAVE
import Data.Char
import Data.List

import Control.Monad
import Control.Monad.Extra


data Tn = Shrt | Lng deriving Show
newtype Ltr = Ltr [Tn] deriving Show
newtype Wd = Wd [Ltr] deriving Show
newtype Txt = Txt [Wd] deriving Show

data WAVs = WAVs {
  short_tone :: WAVE,
  long_tone :: WAVE,
  tone_break :: WAVE,
  letter_break :: WAVE,
  word_break :: WAVE
  }

charToLtr :: Char -> Maybe Ltr
charToLtr c
  | toLower c == 'a' = Just $ Ltr [Shrt, Lng]
  | toLower c == 'b' = Just $ Ltr [Lng,  Shrt, Shrt, Shrt]
  | toLower c == 'c' = Just $ Ltr [Lng,  Shrt, Lng,  Shrt]
  | toLower c == 'd' = Just $ Ltr [Lng,  Shrt, Shrt]
  | toLower c == 'e' = Just $ Ltr [Shrt]
  | toLower c == 'f' = Just $ Ltr [Shrt, Shrt, Lng, Shrt]
  | toLower c == 'g' = Just $ Ltr [Lng,  Lng,  Shrt]
  | toLower c == 'h' = Just $ Ltr [Shrt, Shrt, Shrt, Shrt]
  | toLower c == 'i' = Just $ Ltr [Shrt, Shrt]
  | toLower c == 'j' = Just $ Ltr [Shrt, Lng,  Lng ]
  | toLower c == 'k' = Just $ Ltr [Lng,  Shrt, Lng ]
  | toLower c == 'l' = Just $ Ltr [Shrt, Lng,  Shrt, Shrt]
  | toLower c == 'm' = Just $ Ltr [Lng,  Lng]
  | toLower c == 'n' = Just $ Ltr [Lng, Shrt]
  | toLower c == 'o' = Just $ Ltr [Lng,  Lng,  Lng]
  | toLower c == 'p' = Just $ Ltr [Shrt, Lng,  Lng,  Shrt]
  | toLower c == 'q' = Just $ Ltr [Lng,  Lng,  Shrt, Lng ]
  | toLower c == 'r' = Just $ Ltr [Shrt, Lng,  Shrt]
  | toLower c == 's' = Just $ Ltr [Shrt, Shrt, Shrt]
  | toLower c == 't' = Just $ Ltr [Lng ]
  | toLower c == 'u' = Just $ Ltr [Shrt, Shrt, Lng ]
  | toLower c == 'v' = Just $ Ltr [Shrt, Shrt, Shrt, Lng ]
  | toLower c == 'w' = Just $ Ltr [Shrt, Lng,  Lng]
  | toLower c == 'x' = Just $ Ltr [Lng,  Shrt, Shrt, Lng ]
  | toLower c == 'y' = Just $ Ltr [Lng,  Shrt, Lng,  Lng ]
  | toLower c == 'z' = Just $ Ltr [Lng,  Lng,  Shrt, Shrt]
  | toLower c == 'å' = Just $ Ltr [Shrt, Lng,  Lng,  Shrt, Lng]
  | toLower c == 'ä' = Just $ Ltr [Shrt, Lng,  Shrt, Lng ]
  | toLower c == 'ö' = Just $ Ltr [Lng,  Lng,  Lng,  Shrt]
  | toLower c == 'ß' = Just $ Ltr [Shrt, Shrt, Shrt, Lng,  Lng, Shrt, Shrt]
  | toLower c == 'ü' = Just $ Ltr [Shrt, Shrt, Lng,  Lng ]
  | toLower c == '1' = Just $ Ltr [Shrt, Lng,  Lng,  Lng,  Lng ]
  | toLower c == '2' = Just $ Ltr [Shrt, Shrt, Lng,  Lng,  Lng ]
  | toLower c == '3' = Just $ Ltr [Shrt, Shrt, Shrt, Lng,  Lng ]
  | toLower c == '4' = Just $ Ltr [Shrt, Shrt, Shrt, Shrt, Lng ]
  | toLower c == '5' = Just $ Ltr [Shrt, Shrt, Shrt, Shrt, Shrt]
  | toLower c == '6' = Just $ Ltr [Lng,  Shrt, Shrt, Shrt, Shrt]
  | toLower c == '7' = Just $ Ltr [Lng,  Lng,  Shrt, Shrt, Shrt]
  | toLower c == '8' = Just $ Ltr [Lng,  Lng,  Lng,  Shrt, Shrt]
  | toLower c == '9' = Just $ Ltr [Lng,  Lng,  Lng,  Lng,  Shrt]
  | toLower c == '0' = Just $ Ltr [Lng,  Lng,  Lng,  Lng,  Lng ]
  | toLower c == '?' = Just $ Ltr [Shrt, Shrt, Lng,  Lng,  Shrt, Shrt]
  | toLower c == '!' = Just $ Ltr [Shrt, Shrt, Lng,  Lng,  Shrt]
  | toLower c == ',' = Just $ Ltr [Lng,  Lng,  Shrt, Shrt, Lng,  Lng ]
  | toLower c == '.' = Just $ Ltr [Shrt, Lng,  Shrt, Lng,  Shrt, Lng ]
  | toLower c == '-' = Just $ Ltr [Lng,  Shrt, Shrt, Shrt, Shrt, Lng ]
  | toLower c == '(' = Just $ Ltr [Lng,  Shrt, Lng,  Lng,  Shrt]
  | toLower c == ')' = Just $ Ltr [Lng,  Shrt, Lng,  Lng,  Shrt, Lng ]
  | toLower c == '@' = Just $ Ltr [Shrt, Lng,  Lng,  Shrt, Lng,  Shrt]
  | toLower c == '/' = Just $ Ltr [Lng,  Shrt, Shrt, Lng,  Shrt]
  | toLower c == '%' = Just $ Ltr [Shrt, Lng,  Lng,  Shrt, Shrt]
  | toLower c == '"' = Just $ Ltr [Shrt, Lng,  Shrt, Shrt, Lng,  Shrt]
  | toLower c == '\'' =Just $ Ltr [Shrt, Lng,  Lng,  Lng,  Lng,  Shrt]
  | toLower c == ';' = Just $ Ltr [Lng,  Shrt, Lng,  Shrt, Lng,  Shrt]
  | toLower c == ':' = Just $ Ltr [Lng,  Lng,  Lng,  Shrt, Shrt, Shrt]
  | toLower c == '=' = Just $ Ltr [Lng,  Shrt, Shrt, Shrt, Lng ]
  | toLower c == '+' = Just $ Ltr [Shrt, Lng,  Shrt, Lng,  Shrt]
  | toLower c == '√' = Just $ Ltr [Shrt, Lng,  Shrt, Shrt, Shrt]
  | otherwise = Nothing

stringToTxt :: String -> Maybe Txt
stringToTxt = fmap Txt . mapM (fmap Wd . mapM charToLtr) . words

-- loadWavs :: IO WAVs
-- loadWavs =
--   do
--     short_tone   <- getWAVEFile "wav/short_tone.wav"
--     long_tone    <- getWAVEFile "wav/long_tone.wav"
--     tone_break   <- getWAVEFile "wav/tone_break.wav"
--     letter_break <- getWAVEFile "wav/letter_break.wav"
--     word_break   <- getWAVEFile "wav/word_break.wav"
--     return (WAVs short_tone long_tone tone_break letter_break word_break)

loadWavs :: Int -> Int -> WAVs
loadWavs tone_rate break_rate =
  let
    tone_len = 1200.0/(fromIntegral tone_rate/5)/1000.0 :: Double
    break_len = 1200.0/(fromIntegral break_rate/5)/1000.0 :: Double
    short_tone = generateTone tone_len
    long_tone = generateTone (3 * tone_len)
    tone_break = generateSilence tone_len
    letter_break = generateSilence (3 * break_len)
    word_break = generateSilence (7 * break_len)
  in
    WAVs short_tone long_tone tone_break letter_break word_break
    
txtToWav :: WAVs -> Txt -> Maybe WAVE
txtToWav wavs (Txt wrds) =
  let
    brk = word_break wavs
    wvs  = fmap (intersperse brk) (mapM (wordToWav wavs) wrds) :: Maybe [WAVE]
  in
    maybe Nothing (\w' -> foldl (om concatWaves) (Just $ brk) (w' ++ [brk])) wvs


wordToWav :: WAVs -> Wd -> Maybe WAVE
wordToWav wavs (Wd ltrs) =
  let
    brk = letter_break wavs
    wvs  = fmap (intersperse brk) (mapM (letterToWav wavs) ltrs) :: Maybe [WAVE]
  in
    maybe Nothing (\w' -> foldl (om concatWaves) (Just $ head w') (tail w')) wvs
      
letterToWav :: WAVs -> Ltr -> Maybe WAVE
letterToWav wavs (Ltr snds) =
  let
    shrt = short_tone wavs
    lng  = long_tone wavs
    brk  = tone_break wavs
    (w:wvs)  = intersperse brk $ map sndToWav snds
    sndToWav Shrt = shrt
    sndToWav Lng  = lng
  in
    foldl (om concatWaves) (Just w) wvs
    

concatWaves :: WAVE -> WAVE -> Maybe WAVE
concatWaves (WAVE hdr1 smpls1) (WAVE hdr2 smpls2)
  | compatible hdr1 hdr2 = Just (WAVE (mergeHeader hdr1 hdr2) (smpls1 ++ smpls2))
  | otherwise = Nothing
  where
    compatible (WAVEHeader chnls1 frmr1 bps1 _) (WAVEHeader chnls2 frmr2 bps2 _) = chnls1 == chnls2 && frmr1 == frmr2 && bps1 == bps2
    mergeHeader (WAVEHeader chnls frmr bps frms1) (WAVEHeader _ _ _ frms2) =
      WAVEHeader chnls frmr bps (liftM2 (+) frms1 frms2)

textToMorse :: Int -> Int -> String -> FilePath -> IO ()
textToMorse tone_rate break_rate txt fn =
  do
    let wavs = loadWavs tone_rate break_rate 
    let wf = join $ txtToWav wavs <$> (stringToTxt $ "= " ++ txt ++ " = +")
    case wf of {
      Nothing -> error "Unable to convert text" ;
      Just wf' -> putWAVEFile fn wf'
      }
    

-- | Generate len seconds of square wave
generateTone :: Double -> WAVE
generateTone len =
  let
    max = 2147483647  -- MAXINT32
    min = -2147483648 -- MININT32
    smplcnt = round $ len * 44100
    hdr = WAVEHeader 1 44100 8 (Just smplcnt)
    smpls = take smplcnt $ cycle (replicate 50 [min] ++ replicate 50 [max]) 
  in
    WAVE hdr smpls

-- | Generate len seconds of silence
generateSilence :: Double -> WAVE
generateSilence len =
  let
    smplcnt = round $ len * 44100
    hdr = WAVEHeader 1 44100 8 (Just smplcnt)
    smpls = take smplcnt $ cycle [[0]] 
  in
    WAVE hdr smpls
