{-# LANGUAGE JavaScriptFFI      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

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
import Render
import Logic


foreign import javascript unsafe "clusterData()" clusterData :: IO (T.JSString)
foreign import javascript unsafe "programData()" programData :: IO (T.JSString)


main = do
  !pairs <- dataToPairs 
                      <$> (decodeFromJS <$> programData) 
                      <*> (decodeFromJS <$> clusterData)
  mainWidget $ do

    elClass "div" "container parceed" $ do
      filteredPrograms <- elClass "div" "row" $ do
        filteredPrograms <- elClass "div" "col-md-4" $ do

          filteredPrograms <- el "form" $ do
            specialty <- selectorField "What is your specialty?" (head $ specialties pairs) (constDyn $ specialtiesMap pairs)

            maybeScore1 <- mapDyn readMay =<< textField "What is your USMLE Step 1 Score?"
            maybeScore2 <- mapDyn readMay =<< textField "What is your USMLE Step 2 Score?"


            international <- selectorField "Are you International Medical Graduate (IMG)?" False (constDyn $ M.fromList [(False, "No"), (True, "Yes")])

            input <-  combineDyn (&) international
                  =<< combineDyn (&) specialty
                  =<< combineDyn (\ms1 ms2 -> In (fromMaybe 0 ms1) (fromMaybe 0 ms2) ) maybeScore1 maybeScore2

            programs <- mapDyn (calculate pairs) input
            

            submitEvt <- submitButton "Submit"
            programsDyn <- holdDyn [] $ tagDyn programs submitEvt

            let regionSelectionMap = M.fromList . ((Nothing, "No Filter"):) $ map (\r -> (Just r, unRegion r)) allRegions
            regionToFilterBy <- selectorField "In what US region would you prefer to match?" Nothing (constDyn regionSelectionMap)

            combineDyn (\rf -> filter ((maybe (const True) (flip elem . statesInRegion) rf) . prState)) regionToFilterBy programsDyn
          return filteredPrograms
          


        elAttr "div" ("class" =: "col-md-8") $ do
          elAttr "div" ("id" =: "container") $ text ""

        return filteredPrograms


      elAttr "div" ("class" =: "row") $ do
        elAttr "div" ("class" =: "col-md-12") $ do
          dyn =<< mapDyn renderPrograms filteredPrograms

    return ()

  where
    

    decodeFromJS :: FromJSON a => T.JSString -> a
    decodeFromJS = fromJust . decode . LBS.pack . DT.fromJSString


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
        fromDatum ProgramDatum{pdSchoolName, pdCity, pdState, pdChance, pdLink, pdSpecialty, pdResidencySpecialty} =
          Program pdSchoolName (Specialty pdResidencySpecialty) pdSpecialty (City pdCity) (State pdState) pdChance pdLink 



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





