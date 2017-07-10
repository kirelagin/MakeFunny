{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleInstances, OverloadedStrings, RecordWildCards, TemplateHaskell #-}

module Api where

import Data.Aeson
import Data.Data
import Data.Text
import Data.Time
import Data.Typeable
import GHC.Generics
import Web.Twitter.Conduit
import Web.Twitter.Conduit.Base (endpoint)
import Web.Twitter.Conduit.Parameters
import Web.Twitter.Conduit.Parameters.TH
import Web.Twitter.Types

data UnretweetedStatus =
    UnretweetedStatus
    { ursId              :: StatusId
    , ursText            :: Text
    , ursSource          :: Text
    , ursTruncated       :: Bool
    , ursEntities        :: Maybe Entities
    , ursUser            :: User
    , ursCoordinates     :: Maybe Coordinates
    } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON UnretweetedStatus where
    parseJSON (Object o) = checkError o >>
        UnretweetedStatus <$> o .:  "id"
                          <*> o .:  "text"
                          <*> o .:  "source"
                          <*> o .:  "truncated"
                          <*> o .:? "entities"
                          <*> o .:  "user"
                          <*> o .:? "coordinates"
    parseJSON v = fail $ "couldn't parse unretweeted status from: " ++ show v

instance ToJSON UnretweetedStatus where
    toJSON UnretweetedStatus{..} = object [ "id"                  .= ursId
                                          , "text"                .= ursText
                                          , "source"              .= ursSource
                                          , "truncated"           .= ursTruncated
                                          , "entities"            .= ursEntities
                                          , "user"                .= ursUser
                                          , "coordinates"         .= ursCoordinates
                                          ]

data StatusesUnretweetId
unretweetId :: StatusId -> APIRequest StatusesUnretweetId UnretweetedStatus
unretweetId status_id = APIRequestPost uri def
  where uri = endpoint ++ "statuses/unretweet/" ++ show status_id ++ ".json"
deriveHasParamInstances ''StatusesUnretweetId
    [ "trim_user"
    ]
