module Validations.Adapters.Digestive
  ( validateView
  , validateView'
  , testEnv
  ) 
   where

import Data.Monoid(Monoid, (<>), mempty)
import Text.Digestive.View(View,absolutePath, viewErrors)
import Validations.Validation(Validation, runValidation)
import Data.Text(Text)
import Text.Digestive.Form.Encoding(FormEncType)
import Text.Digestive.Types(Env, FormInput(TextInput), fromPath)

validateView :: (Monad m) => (s -> Validation [(Text, e)] m t u) -> t -> (View e, Maybe s) -> m (View e, Maybe u)
validateView validation t (v,ms) = case ms of
  Nothing -> return $ (v, Nothing)
  Just s  -> do
    (t',es) <- (runValidation (validation s)) t
    es' <- return $  (map (\y -> (absolutePath (fst y) v, snd y)) es) <> (viewErrors v)
    case es of
      [] -> return $ (v, Just t')
      _  -> return $ (v {viewErrors = (viewErrors v) <> es'}, Nothing)

validateView' :: (Monad m, Monoid t) => (s -> Validation [(Text, e)] m t u) -> (View e, Maybe s) -> m (View e, Maybe u)
validateView' validation (v,ms) = validateView validation mempty (v,ms)

testEnv :: Monad m => [(Text, Text)] -> FormEncType -> m (Env m)
testEnv input _formEncType = return $ \key -> return $ map (TextInput . snd) $
    filter ((== fromPath key) . fst) input
