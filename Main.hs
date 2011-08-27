{-# LANGUAGE FlexibleInstances, TemplateHaskell, 
             TupleSections, TypeOperators #-}

import Control.Arrow ((&&&))
import Control.Monad (liftM, zipWithM_)
import Data.Curve
import Data.Curve.Util (zipT)
import Data.Data
import Data.Function (on)
import Data.Generics.Aliases
import Data.Label
import Data.List (groupBy)
import Data.Maybe
import Graphics.ToyFramework
import Language.Haskell.Exts.Annotated
import qualified Graphics.Rendering.Cairo as C

data State = State
  { _code :: String
  , _cursor :: Int
  , _parsed :: (ParseResult (Decl SrcSpanInfo))
  , _mouseCursor :: (Double, Double)
  }

$(mkLabels [''State])

modM :: Monad m => (b :-> a) -> (a -> a) -> b -> m b
modM l f = return . modify l f

setM :: Monad m => (b :-> a) -> a -> b -> m b
setM l x = return . set l x

lensed :: (f :-> a) -> (f :-> a') -> (a -> a') -> f -> f
lensed l l' f s = set l' (f $ get l s) s

updateParse :: State -> State
updateParse = lensed code parsed parseDecl

main :: IO ()
main = runToy $ Toy
  { initialState = updateParse $ 
      State "fibs = 0 : 1 : zipWith (+) fibs (tail fibs)" 0 undefined (0, 220)
  , mouse   = const $ setM mouseCursor
  , key     = handleKey
  , display = handleDisplay
  , tick    = const return
  }

handleKey :: Bool -> Either [Char] Char -> State -> IO State
handleKey True (Right k) (State xs ix p m) =
  return . updateParse $ State (pre ++ (k : post)) (ix + 1) p m
 where 
  (pre, post) = splitAt ix xs

handleKey True (Left k) s@(State xs ix p m) = liftM updateParse $ (case k of
    "Left"  -> modM cursor (max 0 . subtract 1)
    "Right" -> modM cursor (min endPos . (+1))
    "Home"  -> setM cursor 0
    "End"   -> setM cursor endPos
    "BackSpace" -> const (return $ State (delIx (ix - 1)) (max 0 (ix - 1)) p m)
    "Delete" -> setM code (delIx ix)
    "Escape" -> const $ error "User escape"
    _ -> return) s
  where endPos = length xs
        delIx i | (pre, (_:post)) <- splitAt i xs = pre ++ post
                | otherwise = xs

handleKey _ _ s = return s

handleDisplay :: IPnt -> IRect -> State -> C.Render State
handleDisplay _ (tl, br) s@(State txt ix p (_, ypos)) = do
  let textPos = (50.5, 100.5)
      height = (fromIntegral . snd $ br ^-^ tl) * 0.5
      astPos = textPos ^+^ (0.0, ypos - height)

  move textPos
  C.showText txt

  -- Draw the mouse cursor.
  C.setLineWidth 1
  draw . offset (textPos ^+^ (-1, 0)) . rside 1 . expandR 2 =<< textRect txt 0 ix 
  C.stroke
  
  case p of
    ParseOk decl -> drawSpans astPos txt (getSpans decl)
    f@(ParseFailed _ _) -> C.showText (show f)
  C.stroke

  return s

spanLine :: String -> (Int, Int) -> C.Render (Linear Double, Linear Double)
spanLine txt (f, t) = liftM (rside 2 . expandR 2) $ textRect txt f (t - 1)

drawLabeledLine :: String -> DLine -> C.Render ()
drawLabeledLine txt lin = do
  draw lin
  relText 0.5 (lin `at` 0.5 ^-^ (0, 7)) txt

drawSpans :: DPoint  -> String -> [((Int, Int), String)] -> C.Render ()
drawSpans pos txt =
      -- Draw each labeled line, with each subsequent line 15 pixels lower.
  (>>= zipWithM_ (\d (l, n) -> drawLabeledLine n
                             $ offset (pos ^+^ (0, 15) ^* fromIntegral d) l)
                 [0..])

      -- Turn each span into an appropriately sized line segment.
  . mapM (\(s, n) -> liftM (, n) $ spanLine txt s)

      -- Prefer last of all identically-spanned tokens.  Pretty arbitrary.
  . map last . groupBy ((==) `on` (\(x,_)->x))

srcSpan :: SrcSpanInfo -> (Int, Int)
srcSpan = (srcSpanStartColumn &&& srcSpanEndColumn) .  srcInfoSpan

getSpan :: (Data a) => a -> Maybe (Int, Int)
getSpan = listToMaybe . catMaybes . gmapQ (const Nothing `extQ` (Just . srcSpan))

getSpans :: (Data a) => a -> [((Int, Int), String)]
getSpans x = maybeToList (fmap (, show $ toConstr x) $ getSpan x)
          ++ concat (gmapQ getSpans x)
