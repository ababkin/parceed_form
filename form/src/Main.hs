{-# LANGUAGE JavaScriptFFI      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

import Reflex.Dom
import qualified Data.Map as M
import Data.List (nub, foldl')
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
import qualified Data.Set as S
import qualified GHCJS.Types    as T
import qualified GHCJS.DOM.Types    as DT
import qualified GHCJS.Foreign  as F

import Types


foreign import javascript unsafe "clusterData()" clusterData :: IO (T.JSString)
foreign import javascript unsafe "programData()" programData :: IO (T.JSString)

foreign import javascript unsafe "resultStates($1)" resultStates :: T.JSString ->IO ()

dataToPairs :: [ProgramDatum] -> [ClusterDatum] -> [Pair]
dataToPairs pds = M.elems . M.mapWithKey makePair . cdMap
  where
    makePair (cdSpecialty, cdCluster) vs = fromJust $ Pair 
      <$> pure (Specialty cdSpecialty)
      <*> pure (Cluster cdCluster $ programs cdCluster pds)
      <*> lookup "Average Step 1 score min" vs
      <*> lookup "Average Step 1 score max" vs
      <*> lookup "Step 2 Minimum Score" vs
      <*> lookup "H1-B visa" vs
      <*> lookup "J-1 visa" vs
      <*> lookup "Interviews conducted last year for first year positions" vs


    cdMap :: [ClusterDatum] -> M.Map (String, Int) [(String, Double)]
    cdMap = foldl' accum M.empty 
    
    accum !mp ClusterDatum{cdSpecialty, cdCluster, cdIndicator, cdAvgValue} = 
      M.unionWith (++) mp $ M.singleton (cdSpecialty, cdCluster) [(cdIndicator, cdAvgValue)]


programs :: Int -> [ProgramDatum] -> [Program]
programs cl = map fromDatum . filter ((== Just cl) . pdCluster)
  where
    fromDatum :: ProgramDatum -> Program
    fromDatum ProgramDatum{pdSchoolName, pdCity, pdState, pdPrevGMEYears, pdLink, pdResidencySpecialty} =
      Program pdSchoolName (City pdCity) (State pdState) pdPrevGMEYears pdLink (Specialty pdResidencySpecialty)



specialties :: [Pair] -> [Specialty] 
specialties = nub . map pSpecialty 

specialtiesMap :: [Pair] -> M.Map Specialty String
specialtiesMap = M.fromList . map (\s@(Specialty t) -> (s, t)) . specialties



stylesheet :: MonadWidget t m => m ()
stylesheet = elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css") $ return ()

main = do
  !pairs <- dataToPairs 
                      <$> (decodeFromJS <$> programData) 
                      <*> (decodeFromJS <$> clusterData)
  mainWidgetWithHead stylesheet $ do


    elAttr "div" ("class" =: "container") $ do
      elAttr "div" ("class" =: "row") $ do
        elAttr "div" ("class" =: "col-md-4") $ do

          filteredPrograms <- inputForm $ do
            maybeScore1 <- mapDyn readMay =<< textField "Step 1 score"

            maybeScore2 <- mapDyn readMay =<< textField "Step 2 score"


            specialty <- selectorField "Specialty" (head $ specialties pairs) (constDyn $ specialtiesMap pairs)

            international <- selectorField "International" False (constDyn $ M.fromList [(False, "No"), (True, "Yes")])

            maybeExperience <- mapDyn readMay =<< textField "Years of experience"



            input <- combineDyn (\e f -> f (fromMaybe 0 e) ) maybeExperience
              =<< combineDyn (&) international
              =<< combineDyn (&) specialty
              =<< combineDyn (\ms1 ms2 -> In (fromMaybe 0 ms1) (fromMaybe 0 ms2) ) maybeScore1 maybeScore2


            programs <- mapDyn (calculate pairs) input
            statesHist <- mapDyn (M.toList . statesHistogram) programs
            stateSelectionMap <- mapDyn (M.fromList . ((Nothing, "No Filter"):) . map (\(s, n) -> (Just s, unState s ++ " (" ++ show n ++ ")"))) statesHist
            stateToFilterBy <- selectorField "Filter by State" Nothing stateSelectionMap

            combineDyn (\sf -> filter ((maybe (const True) (==) sf) . pState)) stateToFilterBy programs



          dyn =<< mapDyn renderPrograms filteredPrograms

        elAttr "div" ("class" =: "col-md-8") $ do
          elAttr "div" ("id" =: "container") $ text ""

    return ()

decodeFromJS :: FromJSON a => T.JSString -> a
decodeFromJS = fromJust . decode . LBS.pack . DT.fromJSString



{- inputForm = elAttr "form" ("style" =: "width: 600px;") -}
inputForm = el "form"

textField label = do
  elAttr "div" ("class" =: "form-group") $ do
    elAttr "label" ("for" =: "") $ text label
    _textInput_value <$> (textInput $ def & textInputConfig_attributes .~ constDyn ("class" =: "form-control") )


selectorField label z mp = do
  elAttr "div" ("class" =: "form-group") $ do
    elAttr "label" ("for" =: "") $ text label
    _dropdown_value <$> dropdown z mp (def & dropdownConfig_attributes .~ constDyn ("class" =: "form-control") )



{- renderPrograms :: (Reflex t, MonadHold t m) => [Program] -> m () -}
renderPrograms ps = do
  {- elAttr "table" ("style" =: "border-width: 1; border-style: solid") $ do -}
  liftIO . resultStates . DT.toJSString . LBS.unpack . encode . M.mapKeys unState $ statesHistogram ps
  if length ps > 0
    then do
      el "div" . text $ "Found " ++ show resultLen ++ " programs"
      elAttr "table" ("class" =: "table") $ do
        el "tr" $ do
          el "th" $ text "Program name"
        forM_ ps $ \p ->
          el "tr" $ do
            elAttr "a" ( M.fromList [("href", pLink p), ("target", "_blank")] ) . text $ pName p
    else
      el "div" . text $ "No programs were found with this criteria"

  where
    resultLen = length ps


statesHistogram :: [Program] -> M.Map State Int
{- states = S.toList . foldMap (S.singleton . pState) -}
statesHistogram = M.unionsWith (+) . map (\p -> M.singleton (pState p) 1)


calculate :: [Pair] -> In -> [Program]
calculate pairs In{..} = 
  case pFilter pairs of
    [] -> []
    ps -> prFilter . cPrograms . pCluster . maximum $ ps

  where

    pFilter = filter (\p -> pS1MinScore p <= fromIntegral iScore1 && pS1MaxScore p >= fromIntegral iScore1
      && pS2MinScore p <= fromIntegral iScore2
      && pSpecialty p == iSpecialty)

    prFilter = filter (\pr -> (expPred $ pMinYrs pr) && pResidencySpecialty pr == iSpecialty ) 
      
    expPred Nothing = True
    expPred (Just e) = e <= iExp

