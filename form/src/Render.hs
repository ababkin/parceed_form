{-# LANGUAGE JavaScriptFFI      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Render where

import qualified Data.Map as M
import Data.List (nub, foldl', sortBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe, fromJust)
import Safe (readMay)
import Control.Monad (forM_)
import Data.Monoid ((<>))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON),
  Value (..), object, (.:), encode, decode, (.:?))
import Data.Aeson.Types (typeMismatch, Parser, Object)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (sort, maximum)

import Reflex.Dom
import qualified GHCJS.Types    as T
import qualified GHCJS.DOM.Types    as DT
import qualified GHCJS.Foreign  as F

import Types


foreign import javascript unsafe "resultStates($1)" resultStates :: T.JSString ->IO ()

submitButton :: MonadWidget t m => String -> m (Event t ())
submitButton s = do
  elClass "div" "form-group submit" $ do
    (e, _) <- elAttr' "button" (M.fromList [("type", "button"), ("class", "btn btn-primary submit pull-right")]) $ text s
    return $ domEvent Click e


textField label = do
  elClass "div" "form-group" $ do
    elAttr "label" ("for" =: "") $ text label
    _textInput_value <$> textInput (def & textInputConfig_attributes .~ constDyn ("class" =: "form-control") )


selectorField label z mp = do
  elClass "div" "form-group" $ do
    elAttr "label" ("for" =: "") $ text label
    _dropdown_value <$> dropdown z mp (def & dropdownConfig_attributes .~ constDyn ("class" =: "form-control") )



renderPrograms ps = do
  liftIO . resultStates . DT.toJSString . LBS.unpack . encode . M.toList . M.mapKeys unState $ statesHistogram ps
  if length ps > 0
    then do
      el "div" . text $ "Found " ++ show resultLen ++ " programs"
      elClass "table" "table table-striped" $ do
        el "thead" . el "tr" $ do
          el "th" $ text "Program name"
          el "th" $ text "Sub-specialty"
          el "th" $ text "City"
          el "th" $ text "State"
          el "th" $ text "Chance of being accepted"
        el "tbody" $ do
          forM_ ps $ \p ->
            el "tr" $ do
              el "td" . elAttr "a" ( M.fromList [("href", prLink p), ("target", "_blank")] ) . text $ prName p
              el "td" . text $ prSubspecialty p
              el "td" . text . unCity $ prCity p
              el "td" . text . unState $ prState p
              el "td" . text . renderChance $ prChance p
    else
      el "div" . text $ "No programs were found with this criteria"

  where
    resultLen = length ps

    statesHistogram :: [Program] -> M.Map State Int
    statesHistogram = M.unionsWith (+) . map (\p -> M.singleton (prState p) 1)

    renderChance Nothing                = "Unknown"
    renderChance (Just ch)  | ch < 10   = "< 10%"
                            | ch > 90   = "> 90%"
                            | otherwise = show ch ++ "%"

