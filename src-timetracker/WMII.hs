{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable #-}
module WMII where
import Text.ParserCombinators.ReadP
import Control.Monad.Trans
import System.Process
import System.Directory
import System.IO.Unsafe
import System.IO
import Control.Monad
import GHC.Read
import Data.Typeable
import Debug.Trace
import Data.List
import Data.Maybe

import Text.ParserCombinators.ReadP
import Text.Read.Lex as L

import Data.Char
-- ========== events ================================================= 

data WmiiEvent = Key String
	    | FocusTag String
	    | UnfocusTag String
	    | Unkown String -- I don't need that quite now
  deriving (Show)
{-!for WmiiEvent derive: is !-}

-- takeFirst :: [ReadP a] -> ReadP a
-- takeFirst list = look >>= \s -> maybe pfail return $ 
  -- [>take first parse out of first parse of each parser in list as maybe
  -- listToMaybe (foldr (++) ( take 1 (map (\p -> readP_to_S p s) list )) )

class LexValue a where
  lexV :: ReadP a
instance LexValue Int where
  lexV = L.lex >>= \l -> case l of
      L.Int i -> return . fromIntegral $ i
      _ -> pfail
instance LexValue String where
  lexV = L.hsLex

instance Read WmiiEvent where
  readsPrec _ = readP_to_S ( foldr1 (<++)
		[ string "FocusTag" >> skipSpaces >>  fmap FocusTag lexV
		, string "UnfocusTag" >> skipSpaces >> fmap UnfocusTag lexV
		, fmap (Unkown) $ munch (const True) 
		])
    where rp str f = fmap f $ string str >> skipSpaces >> liftM read (many get)
	  rp :: (Read a) => String -> (a -> WmiiEvent) -> ReadP WmiiEvent
	  -- rpUnkown = (many get) >>= return . Unkown
	  -- rpUnkown :: ReadP WmiiEvent


maybeRead s = case reads s of
        [(x, s')] | all isSpace s' -> Just x
        _                          -> Nothing


wmiiActionsAsStringList  =  liftM lines $ wmiir_get "read" "/event"
wmiiActions :: IO [WmiiEvent]
wmiiActions = liftM (map read_no_faill ) $ wmiiActionsAsStringList
  where read_no_faill s = 
              case maybeRead s of
                Just x -> x
                Nothing -> Unkown $ "error parsing " ++ (show s)


-- =============  =======================================================

type Tag = String

newtype TagList = TagList [Tag]
  deriving (Typeable,Eq)
type View = TagList

unTagList (TagList l) = l

appendTag (TagList l) t = TagList $ l++[t]
mergeTagLists (TagList a) (TagList b)  = TagList $ a ++ b

-- incomplete on [a,b,delemiter]
splitList :: (Show a,Eq a) => [a] -> [a] -> [[a]]
splitList d l = sl [] l
  where sl a [] = (reverse a):[]
        sl a l  = let (isP, rest@(r:rs)) = eatPrefix d l
		  in if isP then (reverse a):(sl [] rest) 
			    else sl (r:a) rs
	eatPrefix p l = let result = if isPrefixOf p l then (True, drop (length p) l)
						       else (False, l)
			in result


  
-- opposite of splitList
joinList :: [a] -> [[a]] -> [a]
joinList _ [] = []
joinList _ (a:[]) = a
joinList d l= foldr1 (\a b -> a++d++b) l

-- these functions escape the + sign in a weired way
viewToEscapedString :: TagList -> String
viewToEscapedString (TagList t) = joinList "_" t
escapedStringToView :: String -> TagList
escapedStringToView l = TagList $ splitList "_" l

instance Show TagList where
  show (TagList list) = joinList "+" list
instance Read TagList where
  readsPrec _ = (\str -> [(TagList $ lines $ map (\a -> if a == '+' then '\n' else a) str,"")])
  --splitOnPlus
  --where splitOnPlus :: String -> [String]
	--splitOnPlus ""	=  []
	--splitOnPlus s	=  let (l, s') = break (== '+') s
			   --in  l : case s' of
					  --[] -> []
					  --(_:s'') -> splitOnPlus s''

wmiir = fromJust $ unsafePerformIO $ findExecutable $ "wmiir"
-- "/usr/local/bin/wmiir"
--wmiir = "/pr/haskell/wmii/wmiirfake"


wmiir_get :: String -> String -> IO String
wmiir_get read_type path = do
  unless (read_type `elem ` ["read","ls"]) $ fail "wrong read type, only ls or read supported"
  (in_,out,err,ph) <- runInteractiveProcess wmiir [read_type,path] Nothing Nothing
  result <- hGetContents out
  mapM hClose [in_,err]
  return result

wmiir_read = wmiir_get "read"
wmiir_ls = wmiir_get "ls"

wmiir_write :: String -> String -> IO ()
wmiir_write path value = do
  print $ "target path: "++path++" value"++value
  (in_,out,err,ph) <- runInteractiveProcess wmiir ["write", path] Nothing Nothing
  hPutStr in_ value
  hFlush in_
  mapM_ hClose [in_,out,err]
  waitForProcess ph
  return ()
  

view_path  = "/tag/sel/ctl/tags"
view :: IO View
view = liftM (read.(drop $ length "view ").head.lines) $ wmiir_read "/ctl/"
listViews :: IO [TagList]
listViews = liftM (filter (/= TagList ["sel"]).(map read).(map init).lines) $ wmiir_ls "/tag"
setView :: TagList -> IO ()
setView view = wmiir_write "/ctl/" $ "view "++show view
tags_path = "/client/sel/tags"
clientTags :: IO TagList
clientTags = liftM read $ wmiir_read  tags_path 
setClientTags :: TagList -> IO ()
setClientTags tags = wmiir_write tags_path $ show tags

