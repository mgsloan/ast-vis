{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, FlexibleInstances,
TemplateHaskell, MultiParamTypeClasses, TypeSynonymInstances, TupleSections #-}

import Control.Monad (liftM, zipWithM)
import Control.Arrow ((&&&))
import Data.Monoid (mconcat)

import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Annotated.Syntax
import qualified Graphics.Rendering.Cairo as C

import Data.Curve
import qualified Data.Curve.Interval as I

import Graphics.ToyFramework

import Debug.Trace

import Data.Maybe
import Data.Either
import Data.Either.Unwrap
import Data.List (groupBy)
import Data.Function (on)

import Data.Data
import Data.DeriveTH
-- import Data.Generics.PlateTypeable
import Data.Generics.Uniplate.Data
import GHC.Real (Ratio(..))
import Data.Generics.Aliases
import Text.ParserCombinators.ReadP

{-
$( derives [makeUniplateTypeable] [''Decl, ''SrcSpan, ''SrcSpanInfo,
  -- Immediate dependencies of Decl
  ''Kind, ''FunDep, ''Op, ''Match, ''Rhs, ''CallConv, ''QName, ''Annotation,
  ''Activation, ''Rule, ''Exp, ''ClassDecl, ''InstDecl, ''Pat, ''Name, ''Context,
  ''InstHead, ''Type, ''Safety, ''Binds, ''Assoc, ''DeclHead, ''DataOrNew,
  ''GadtDecl, ''QualConDecl,
  
  -- Dependencies of Exp
  ''Alt, ''QOp, ''XAttr, ''Splice, ''FieldUpdate, ''Literal, ''QualStmt, ''XName,
  ''Bracket, ''Stmt, ''IPName,

  ''TyVarBind, ''IPBind, ''ConDecl, ''FieldDecl, ''BangType,
  ''GuardedAlts, ''Boxed, ''Asst, ''PXAttr, ''PatField, ''RPat, ''Deriving,
  ''RuleVar, ''SpecialCon, ''ModuleName, ''GuardedRhs, ''GuardedAlt, ''RPatOp
  ] )
-}

-- TySynD GHC.Real.Rational [] (AppT (ConT GHC.Real.Ratio) (ConT GHC.Integer.Type.Integer))

--instance (Typeable a, Uniplate a) => PlateAll Rational a where
--  plateAll (x :% y) = plate (:%) |+ x |+ y

main = runToy $ Toy
  { initialState = initial
  , display = handleDisplay
  , key     = handleKey
  , mouse   = handleMouse
  , tick    = update
  }

data State = State String Int (ParseResult (Decl SrcSpanInfo))

initial = State "1 + 1" 0 (ParseFailed (SrcLoc "" 0 0)  "")

--hline x1 x2 y = ((x1, y), (x2, y))
--vline x y1 y2 = ((x, y1), (x, y2))

gshows2 :: Data a => Int -> a -> ShowS

gshows2 depth = ( \t ->
                showString ("(" ++ show depth)
              . (showString . showConstr . toConstr $ t)
              . (foldr (.) id . gmapQ ((showChar ' ' .) . gshows2 (depth + 1) ) $ t)
              . showChar ')'
         ) `extQ` (shows :: String -> ShowS)
           `extQ` (shows . srcSpan)

single x = [x]

{-
instance (Monoid a, Monoid b) => Monoid (Either a b) where
  mempty = Left mempty
  mappend (Left x) ()
  mappend (Left
 -}


debug x = trace (show x) x

glines :: (Data a) => Int -> a -> [Maybe (Either (Int, Int) ((Int, Int), Int, String))]
glines depth = (\t -> (\xs -> helper (show $ toConstr t) xs : xs)
                      . concat . gmapQ (glines $ depth + 1) $ t)
        `extQ` (single . Just . Left . srcSpan)
  where helper name xs = (listToMaybe . lefts $ catMaybes xs) >>=
                         return . Right . (, depth, name)


srcSpan = (srcSpanStartColumn &&& srcSpanEndColumn) . srcInfoSpan

instance Draw (String, DLine) where
  draw (txt, x) = draw x >> move (x `at` 0.5) >> C.showText txt

handleDisplay :: IPnt -> IRect -> State -> C.Render State
handleDisplay _ _ s@(State xs ix p) = do
  C.moveTo 10 200
  C.showText xs
  (textRect (10.0, 200.0) xs 0 ix) >>= draw . rside 1
  C.stroke
  
  C.moveTo 10 150
  case p of
    ParseOk x -> (toSegs (0.0, 220.0) . process . rights . catMaybes $ glines 0 x)
             >>= mapM_ draw
    f@(ParseFailed _ _) -> C.showText (show f)
  C.stroke

  return s
 where
  process = map last . groupBy ((==) `on` (\(x,_,_)->x))
  toSegs p = zipWithM (\d ((f, t), _, txt) -> liftM ((txt,) . rside 0) $
               textRect (p ^+^ ((0,15) ^* (fromIntegral d))) xs f t) [1..]

handleKey :: Either [Char] Char -> Bool -> State -> IO State
handleKey (Right k) True = return . updateParse .
                           (\(State x ix p) -> State (insIx [k] ix x) (ix + 1) p)
handleKey (Left k) True = liftM updateParse . ((case k of
    "Left" -> modIx (max 0 . subtract 1)
    "Right" -> modIx (1+)
    "BackSpace" -> return . (\(State x ix p) -> State (delIx (ix - 1) x) (max 0 (ix - 1)) p)
    "Delete" -> return . (\(State x ix p) -> State (delIx ix x) ix p)
    "Escape" -> error "User escape"
    _ -> return) :: State -> IO State)
  where modIx :: (Int -> Int) -> State -> IO State
        modIx f (State x ix p) = return $ State x (f ix) p
handleKey _ _ = return

updateParse (State str ix _) = State str ix (parseDecl str)

handleMouse :: Maybe (Bool, Int) -> DPoint -> State -> IO State
handleMouse _ _ = return

update _ s = return s

-- Util
insIx v i xs = pre ++ v ++ post     where (pre, post)     = splitAt i xs
delIx i xs | (pre, (x:post)) <- splitAt i xs = pre ++ post
           | otherwise = xs

textRect :: DPoint -> String -> Int -> Int -> C.Render DRect
textRect (x, y) txt 0 t = do
  (C.TextExtents _ _ _ _ w h) <- C.textExtents (take t txt)
  return $ (x + 2 I.... x + w, y - 10 I.... y + 2)
textRect (x, y) txt f t | t >= f = do
  (C.TextExtents _ _ _ _ w1 _) <- C.textExtents pre
  (C.TextExtents _ _ _ _ w2 _) <- C.textExtents (take (t - f) post)
  return $ (x + w1 + 2 I.... x + w1 + w2, y - 10 I.... y + 2)
 where (pre, post) = splitAt f txt
