{-# LANGUAGE JavaScriptFFI      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

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

import Reflex.Dom
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
      <*> lookup "Percent of IMG" vs
      <*> lookup "Interviews conducted last year for first year positions" vs


    cdMap :: [ClusterDatum] -> M.Map (String, Int) [(String, Double)]
    cdMap = foldl' accum M.empty 
    
    accum !mp ClusterDatum{cdSpecialty, cdCluster, cdIndicator, cdAvgValue} = 
      M.unionWith (++) mp $ M.singleton (cdSpecialty, cdCluster) [(cdIndicator, cdAvgValue)]


programs :: Int -> [ProgramDatum] -> [Program]
programs cl = sort . map fromDatum . filter ((== Just cl) . pdCluster)
  where
    fromDatum :: ProgramDatum -> Program
    fromDatum ProgramDatum{pdSchoolName, pdCity, pdState, pdPrevGMEYears, pdLink, pdResidencySpecialty} =
      Program pdSchoolName (City pdCity) (State pdState) pdPrevGMEYears pdLink (Specialty pdResidencySpecialty)



specialties :: [Pair] -> [Specialty] 
specialties = nub . map pSpecialty 

specialtiesMap :: [Pair] -> M.Map Specialty String
specialtiesMap = M.fromList . map (\s@(Specialty t) -> (s, t)) . specialties


allRegions = map Region ["Northeast Region", "Midwest Region", "South Region", "West Region"]

statesInRegion :: Region -> [State]
statesInRegion = map State . statesMap
  where
    statesMap r | r == Region "Northeast Region"  = ["Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island",
                                                      "Vermont", "New Jersey", "New York", "Pennsylvania"]
                | r == Region "Midwest Region"    = ["Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin",
                                                      "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota"]
                | r == Region "South Region"      = ["Delaware", "District of Columbia", "Florida", "Georgia", "Maryland", "North Carolina", 
                                                      "South Carolina", "Virginia", "West Virginia", "Alabama", "Kentucky", "Mississippi",
                                                      "Tennessee", "Arkansas", "Louisiana", "Oklahoma", "Texas"]
                | r == Region "West Region"       = ["Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", 
                                                      "Wyoming", "Alaska", "California", "Hawaii", "Oregon", "Washington"]


stylesheet :: MonadWidget t m => m ()
stylesheet = elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css") $ return ()

main = do
  !pairs <- dataToPairs 
                      <$> (decodeFromJS <$> programData) 
                      <*> (decodeFromJS <$> clusterData)
  mainWidgetWithHead stylesheet $ do


    elAttr "div" ("class" =: "container") $ do
      filteredPrograms <- elAttr "div" ("class" =: "row") $ do
        filteredPrograms <- elAttr "div" ("class" =: "col-md-4") $ do

          filteredPrograms <- el "form" $ do
            specialty <- selectorField "What is your specialty?" (head $ specialties pairs) (constDyn $ specialtiesMap pairs)

            maybeScore1 <- mapDyn readMay =<< textField "What is your USMLE Step 1 Score?"
            maybeScore2 <- mapDyn readMay =<< textField "What is your USMLE Step 2 Score?"


            international <- selectorField "Are you International Medical Graduate (IMG)?" False (constDyn $ M.fromList [(False, "No"), (True, "Yes")])

            maybeExperience <- mapDyn readMay =<< textField "How many years of clinical experience do you have?"

            input <- combineDyn (\e f -> f (fromMaybe 0 e) ) maybeExperience
              =<< combineDyn (&) international
              =<< combineDyn (&) specialty
              =<< combineDyn (\ms1 ms2 -> In (fromMaybe 0 ms1) (fromMaybe 0 ms2) ) maybeScore1 maybeScore2

            programs <- mapDyn (calculate pairs) input
            
            {- statesHist <- mapDyn (M.toList . statesHistogram) programs -}


            {- regionSelectionMap <- mapDyn (M.fromList . ((Nothing, "No Filter"):) . map (\(s, n) -> (Just s, unState s ++ " (" ++ show n ++ ")"))) statesHist -}
            let regionSelectionMap = M.fromList . ((Nothing, "No Filter"):) $ map (\r -> (Just r, unRegion r)) allRegions
            regionToFilterBy <- selectorField "In what US region would you prefer to match?" Nothing (constDyn regionSelectionMap)

            combineDyn (\rf -> filter ((maybe (const True) (flip elem . statesInRegion) rf) . pState)) regionToFilterBy programs
          return filteredPrograms
          


        elAttr "div" ("class" =: "col-md-8") $ do
          elAttr "div" ("id" =: "container") $ text ""

        return filteredPrograms


      elAttr "div" ("class" =: "row") $ do
        elAttr "div" ("class" =: "col-md-12") $ do
          dyn =<< mapDyn renderPrograms filteredPrograms

    return ()

decodeFromJS :: FromJSON a => T.JSString -> a
decodeFromJS = fromJust . decode . LBS.pack . DT.fromJSString


textField label = do
  elAttr "div" ("class" =: "form-group") $ do
    elAttr "label" ("for" =: "") $ text label
    _textInput_value <$> textInput (def & textInputConfig_attributes .~ constDyn ("class" =: "form-control") )


selectorField label z mp = do
  elAttr "div" ("class" =: "form-group") $ do
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
          el "th" $ text "State"
        el "tbody" $ do
          forM_ ps $ \p ->
            el "tr" $ do
              el "td" . elAttr "a" ( M.fromList [("href", pLink p), ("target", "_blank")] ) . text $ pName p
              el "td" . text . unState $ pState p
    else
      el "div" . text $ "No programs were found with this criteria"

  where
    resultLen = length ps


statesHistogram :: [Program] -> M.Map State Int
statesHistogram = M.unionsWith (+) . map (\p -> M.singleton (pState p) 1)


calculate :: [Pair] -> In -> [Program]
calculate pairs In{..} = 
  case pScoreFilter pairs of
    [] -> []
    ps -> prFilter . cPrograms . pCluster . pMaxFilter iIntl $ ps

  where
    maximumByInterviews = unPairSortableByInterviews . maximum . map PairSortableByInterviews
    maximumByIMGProb = unPairSortableByIMGProb . maximum . map PairSortableByIMGProb

    pScoreFilter = filter (\p -> pS1MinScore p <= fromIntegral iScore1 && pS1MaxScore p >= fromIntegral iScore1
      && pS2MinScore p <= fromIntegral iScore2
      && pSpecialty p == iSpecialty)

    pMaxFilter False = maximumByInterviews
    pMaxFilter True = maximumByIMGProb

    prFilter = filter (\pr -> (expPred $ pMinYrs pr) && pResidencySpecialty pr == iSpecialty ) 
      
    expPred Nothing = True
    expPred (Just e) = e <= iExp

