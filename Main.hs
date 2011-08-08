{-# LANGUAGE FlexibleInstances, TupleSections #-}

import Control.Monad (liftM, zipWithM)
import Control.Arrow ((&&&), first)

import Language.Haskell.Exts.Annotated
import qualified Graphics.Rendering.Cairo as C

import Data.Curve
import qualified Data.Curve.Interval as I

import Graphics.ToyFramework

import Data.Maybe
import Data.Either
import Data.List (groupBy)
import Data.Function (on)

import Data.Data
import Data.Generics.Aliases

main = runToy $ Toy
  { initialState = State "f = a . b . c $ 4 * 2 + 1" 0
      (ParseFailed (SrcLoc "" 0 0)  "")
      (0, 220)
  , mouse   = handleMouse
  , key     = handleKey
  , display = handleDisplay
  , tick    = const return
  }

handleMouse :: Maybe (Bool, Int) -> DPoint -> State -> IO State
handleMouse _ d (State a b c _) = return $ State a b c d

updateParse (State str ix _ m) = State str ix (parseDecl str) m

handleKey :: Either [Char] Char -> Bool -> State -> IO State
handleKey (Right k) True (State x ix p m) =
  return . updateParse $ State (insIx [k] ix x) (ix + 1) p m

handleKey (Left k) True s@(State x ix p m) = liftM updateParse $ (case k of
    "Left"  -> modIx (max 0 . subtract 1)
    "Right" -> modIx (min endPos . (+1))
    "Home"  -> modIx (const 0)
    "End"   -> modIx (const endPos)
    "BackSpace" -> return $ State (delIx (ix - 1) x) (max 0 (ix - 1)) p m
    "Delete" -> return $ State (delIx ix x) ix p m
    "Escape" -> error "User escape"
    _ -> return s)
  where modIx f = return $ State x (f ix) p m
        endPos = length x

handleKey _ _ s = return s


handleDisplay :: IPnt -> IRect -> State -> C.Render State
handleDisplay _ (tl, br) s@(State xs ix p m) = do
  C.moveTo 10 200
  C.showText xs
  (textRect (10.0, 200.0) xs 0 ix) >>= draw . rside 1
  C.stroke
  
  C.moveTo 10 150
  case p of
    ParseOk x -> (toSegs ((10.0, snd m - fromIntegral height * 0.5))
               . process . rights . catMaybes $ glines 0 x)
             >>= mapM_ draw
    f@(ParseFailed _ _) -> C.showText (show f)
  C.stroke

  return s
 where
  process = map last . groupBy ((==) `on` (\(x,_,_)->x))
  toSegs p = zipWithM (\d ((f, t), _, txt) ->
               liftM ((txt,) . rside 0 . first (I.expand 2)) $
               textRect (p ^+^ ((0,15) ^* (fromIntegral d))) xs (f-1) (t-1)) [1..]
  height = snd $ br ^-^ tl
data State = State String Int (ParseResult (Decl SrcSpanInfo)) (Double, Double)

srcSpan = (srcSpanStartColumn &&& srcSpanEndColumn) . srcInfoSpan

glines :: (Data a) => Int -> a -> [Maybe (Either (Int, Int) ((Int, Int), Int, String))]
glines depth = (\t -> (\xs -> helper (show $ toConstr t) xs : xs)
                      . concat . gmapQ (glines $ depth + 1) $ t)
        `extQ` (single . Just . Left . srcSpan)
  where helper name xs = (listToMaybe . lefts $ catMaybes xs) >>=
                         return . Right . (, depth, name)

-- Util
single x = [x]
insIx v i xs = pre ++ v ++ post
  where (pre, post) = splitAt i xs
delIx i xs | (pre, (_:post)) <- splitAt i xs = pre ++ post
           | otherwise = xs

instance Draw (String, DLine) where
  draw (txt, x) = draw x >> move (x `at` 0.5) >> C.showText txt

textRect :: DPoint -> String -> Int -> Int -> C.Render DRect
textRect (x, y) txt f t | t >= f = do
  (C.TextExtents _ _ _ _ w1 _) <- C.textExtents pre
  (C.TextExtents _ _ _ _ w2 _) <- C.textExtents (take (t - f) post)
  return $ (x + w1 + 2 I.... x + w1 + w2, y - 10 I.... y + 2)
 where (pre, post) = splitAt f txt
