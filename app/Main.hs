{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import BasePrelude
import Web.Spock
import Web.Spock.Config 
import Web.Spock.Lucid (lucid)
import Lucid 
import Data.Text (Text, pack)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import System.Random
import qualified Database.PostgreSQL.Simple          as PG
import qualified Database.PostgreSQL.Simple.FromRow  as PG
import qualified Database.PostgreSQL.Simple.ToRow    as PG 
import qualified Database.PostgreSQL.Simple.ToField  as PG
import qualified Data.Configurator as C

main :: IO ()
main = do 
    connInfo <- loadConnInfo "db.cfg"
    conn <- PG.connect connInfo
    st <- ServerState <$> 
        newIORef [ Note "Alice" "Must not forget Lily." 0 
                 , Note "Bob" "Must. Get. Doggo." 1
        ]
    cfg <- defaultSpockCfg () (PCConn (mkConnBuilder connInfo)) st
    runSpock 8080 (spock cfg myapp)

type Server a = SpockM () () ServerState a 

myapp :: SpockM PG.Connection () ServerState ()
myapp = do 
    get root $ do
        -- notes' <- getState >>= (liftIO . readIORef . notes)
        notes' <- runQuery (\conn -> allNotes conn)
        lucid $ do
            notesHTML notes' 
            newNotesForm
    post root $ do 
        author <- param' "author"
        contents <- param' "contents" 
        _id <- liftIO $ randomIO
        _ <- runQuery $ insertNote $ Note author contents _id
        notesRef <- notes <$> getState
        liftIO $ atomicModifyIORef' notesRef $ \notes -> 
            (notes <> [Note author contents _id], ())
        redirect "/"
    -- delete root $ do 
    --     notesRef <- notes <$> getState 
    --     liftIO $ atomicModifyIORef' notesRef $ \notes -> 
    --         (filter (_id /= ))

notesHTML :: [Note] -> Html ()
notesHTML notes' = do
    h1_ "Notes"
    ul_ $ forM_ notes' $ \note -> li_ $ do 
        toHtml (author note) 
        ": "
        toHtml (contents note)
        button_ [method_ "delete" , id_ ((pack . show) (_id note))] "Delete"

newNotesForm :: Html ()
newNotesForm = do 
    h2_ "New Note" 
    form_ [method_ "post"] $ do 
        label_ $ do 
            "Author: " 
            input_ [name_ "author"]
        label_ $ do 
            "Contents: "
            textarea_ [name_ "contents"] ""
        input_ [type_ "submit", value_ "Add Note"]

newtype ServerState = ServerState { notes :: IORef [Note] }
data Note = Note { author :: Text, contents :: Text, _id :: Integer }

insertNote :: Note -> PG.Connection -> IO Int64 
insertNote n conn = 
    PG.executeMany
        conn
        "INSERT INTO note (author, contents, _id) VALUES (?, ?, ?)"
        [n]

allNotes :: PG.Connection -> IO [Note] 
allNotes c = PG.query_ c "SELECT author, contents, _id FROM note"

instance PG.ToRow Note where 
    toRow n = PG.toField <$> [author n, contents n, pack $ show $_id n]

instance PG.FromRow Note where 
    fromRow = Note <$> PG.field <*> PG.field <*> PG.field 

loadConnInfo :: FilePath -> IO PG.ConnectInfo
loadConnInfo path = do 
    dbCfg <- C.load $ [C.Required path]
    PG.ConnectInfo <$> C.require dbCfg "host"
                <*> C.require dbCfg "port"
                <*> C.require dbCfg "user"
                <*> C.require dbCfg "password"
                <*> C.require dbCfg "db"

mkConnBuilder :: PG.ConnectInfo -> ConnBuilder PG.Connection
mkConnBuilder connInfo = 
    ConnBuilder { cb_createConn = PG.connect connInfo
                , cb_destroyConn = PG.close 
                , cb_poolConfiguration = 
                    PoolCfg { pc_stripes = 1
                            , pc_resPerStripe = 5 
                            , pc_keepOpenTime = 60 }
                }