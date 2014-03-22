module Validations.Adapters.Digestive
  ( validateView
  , testEnv
  ) 
   where

import Data.Monoid(Monoid,(<>))
import Text.Digestive.View(View,absolutePath,viewErrors)
import Validations.Validation(Validation,runValidation')
import Data.Text(Text)
import Text.Digestive.Form.Encoding(FormEncType)
import Text.Digestive.Types(Env,FormInput(TextInput),fromPath)

validateView :: (Monad m, Monoid v, Monoid t) => (s -> Validation Text v t m a b) -> m (View v, Maybe s) -> m (View v, Maybe t)
validateView validation view  = view >>= \(v,x) -> case x of
  Nothing -> return $ (v, Nothing)
  Just s  -> do
    (s',es) <- runValidation' (validation s) []
    es' <- return $  (map (\y -> (absolutePath (fst y) v, snd y)) es) <> (viewErrors v)
    case es of
      [] -> return $ (v, Just s')
      _  -> return $ (v {viewErrors = (viewErrors v) <> es'}, Nothing)

testEnv :: Monad m => [(Text, Text)] -> FormEncType -> m (Env m)
testEnv input _formEncType = return $ \key -> return $ map (TextInput . snd) $
    filter ((== fromPath key) . fst) input
