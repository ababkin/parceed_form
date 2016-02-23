{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE RecordWildCards   #-}

import           Control.Applicative        ((<$>), (<*>))
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Csv as CSV
import           Data.List                  (intercalate)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           System.Environment         (getArgs)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Object),
                   eitherDecode, encode, object, (.:), (.=))
import Data.Aeson.Types (typeMismatch)


data ClusterDatum = ClusterDatum {
    cdSpecialty :: String
  , cdCluster   :: String
  , cdIndicator :: String
  , cdAvgValue  :: String
  }
instance ToJSON ClusterDatum where
  toJSON (ClusterDatum {..})  =
    object  [
              "specialty" .= cdSpecialty
            , "cluster"   .= cdCluster
            , "indicator" .= cdIndicator
            , "avgValue"  .= cdAvgValue
            ]

data ProgramDatum = ProgramDatum {
    pdId                 :: String
  , pdSchoolName         :: String
  , pdSpecialty          :: String
  , pdCity               :: String
  , pdState              :: String
  , pdResidencySpecialty :: String
  , pdPrevGMEYears       :: String
  , pdCluster            :: String
  , pdLink               :: String
  }
instance ToJSON ProgramDatum where
  toJSON (ProgramDatum {..})  =
    object  [
              "id"  .= pdId
            , "schoolName" .= pdSchoolName
            , "specialty" .= pdSpecialty
            , "city" .= pdCity
            , "state" .= pdState
            , "residencySpecialty" .= pdResidencySpecialty
            , "prevGMEYears" .= pdPrevGMEYears
            , "cluster" .= pdCluster
            , "link" .= pdLink
            ]



main :: IO ()
main = do

  process "clusters" extractCluster 
  process "programs" extractProgram 

  where
    extractCluster :: [BS.ByteString] -> ClusterDatum
    extractCluster fields = ClusterDatum cdSpecialty cdCluster cdIndicator cdAvgValue
      where
          [ cdSpecialty, cdCluster, cdIndicator, cdAvgValue] = map BS.unpack fields



    extractProgram :: [BS.ByteString] -> ProgramDatum
    extractProgram fields = ProgramDatum pdId pdSchoolName pdSpecialty pdCity 
                              pdState pdResidencySpecialty pdPrevGMEYears pdCluster pdLink
      where
          [pdId, pdSchoolName, pdSpecialty, pdCity, pdState, pdResidencySpecialty, 
            pdPrevGMEYears, pdCluster, pdLink] = map BS.unpack fields


  
    process name extract = do
      content <- BL.readFile $ name ++ ".csv"
      case CSV.decode CSV.HasHeader content :: Either String (Vector (Vector BS.ByteString)) of
        Right rows -> do
          let clusters = map extract (V.toList . V.map V.toList $ rows)
          BL.writeFile (name ++ ".json") $ encode clusters

        Left err ->
          putStrLn $ "could not extract " ++ name ++ " rows: " ++ show err


