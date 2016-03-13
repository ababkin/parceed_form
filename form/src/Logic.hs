{-# LANGUAGE JavaScriptFFI      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Logic where

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
import Data.List (sort, maximum, nub, foldl', sortBy)

import Types


calculate :: [Pair] -> In -> [Program]
calculate pairs In{iSpecialty, iScore1, iScore2, iIntl} = 
  case pScore2Filter . pScore1Filter . pSpecialtyFilter $ pairs of
    [] -> []
    ps -> prFilter . cPrograms . pCluster . pMaxFilter iIntl $ ps

  where

    pScore1Filter = 
      scoreFilter 
        (\p -> pS1MinScore p <= fromIntegral iScore1 && pS1MaxScore p >= fromIntegral iScore1)
        (\p -> abs( (pS1MinScore p + pS1MaxScore p) / 2 - fromIntegral iScore1) )

    pScore2Filter = 
      scoreFilter 
        (\p -> pS2MinScore p <= fromIntegral iScore2)
        (\p -> abs( pS2MinScore p - fromIntegral iScore2) )

    scoreFilter criteria ordering ps = 
      case filter criteria ps of
        [] -> take 1 $ sortBy (comparing ordering) ps
        fps -> fps


    pSpecialtyFilter = filter (\p -> pSpecialty p == iSpecialty)


    pMaxFilter False = unPairSortableByInterviews . maximum . map PairSortableByInterviews
    pMaxFilter True = unPairSortableByIMGProb . maximum . map PairSortableByIMGProb


    prFilter = filter (\pr -> prSpecialty pr == iSpecialty ) 
