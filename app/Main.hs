{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           BasePrelude                        hiding (delete)
import           Control.Monad                      (forM_)
import           Control.Monad.IO.Class             (liftIO)
import qualified Data.Configurator                  as C
import           Data.Text                          (Text, pack)
import qualified Database.PostgreSQL.Simple         as PG
import qualified Database.PostgreSQL.Simple.FromRow as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Database.PostgreSQL.Simple.ToRow   as PG
import           Lucid
import           Web.Spock
import           Web.Spock.Config
import           Web.Spock.Lucid                    (lucid)

main :: IO ()
main
  -- Load the db config
 = do
  connInfo <- loadConnInfo databaseConfigFile
  -- Connect to postgres
  conn <- PG.connect connInfo
  liftIO $ createNoteTable conn
  -- Initialize a state variable for the app
  st <- MkApp <$> newIORef initialAppState
  -- Get spock config
  cfg <- defaultSpockCfg EmptySession (PCConn (mkConnBuilder connInfo)) st
  -- Run the app
  runSpock 8080 (spock cfg myapp)

type Server a = SpockM () () App a

databaseConfigFile :: String
databaseConfigFile = "db.cfg"

initialAppState :: AppState
initialAppState = MkAppState {notes = [], numNotes = 0}

data MySession =
  EmptySession

myapp :: SpockM PG.Connection MySession App ()
myapp = do
  existingNotes <- runQuery allNotes
  -- Fill in the inital app state
  (MkApp ref) <- getState
  liftIO $
    writeIORef
      ref
      MkAppState
        {notes = existingNotes, numNotes = fromIntegral $ length existingNotes}
  get root $ do
    (MkApp ref) <- getState
    appState' <- liftIO $ readIORef ref
    lucid $ do
      notesHTML $ notes appState'
      newNotesForm
      deleteTableButton_
  postNote
  deleteNote
  deleteEverything
  sortAscNotes
  getNotesByQuery sortedByContent
  get "error" $ text $ pack $ "bad note id, sorry"

getNotesByQuery :: (PG.Connection -> IO [Note]) -> SpockCtxM ctx PG.Connection sess App ()
getNotesByQuery query =
  get "wellThisSucks" $ do
    sortedNotes <- runQuery query
    (MkApp ref) <- getState
    liftIO $
      writeIORef
        ref
        MkAppState
          {notes = sortedNotes, numNotes = fromIntegral $ length sortedNotes}
    redirect "/"

sortAscNotes :: SpockCtxM ctx PG.Connection sess App ()
sortAscNotes =
  get "sortAsc" $ do
    sortedNotes <- runQuery sortedAscNotes
    (MkApp ref) <- getState
    liftIO $
      writeIORef
        ref
        MkAppState
          {notes = sortedNotes, numNotes = fromIntegral $ length sortedNotes}
    redirect "/"

deleteEverything :: SpockCtxM ctx PG.Connection sess App ()
deleteEverything =
  get "deleteAll" $
    -- Empty the notes table by dropping it
   do
    (MkApp ref) <- getState
    liftIO $ writeIORef ref initialAppState
    _ <- runQuery deleteNoteTable
    -- Recreate the table so that it can be queried
    _ <- runQuery createNoteTable
    redirect "/"

deleteNote :: SpockCtxM ctx PG.Connection sess App ()
deleteNote =
  get ("remove" <//> var) $ \noteIdTxt -> do
    (MkApp ref) <- getState
    case (readMaybe noteIdTxt) :: Maybe Integer of
      Just noteId -> do
        liftIO $
          atomicModifyIORef'
            ref
            (\as -> (removeNote noteId as, removeNote noteId as))
        _ <- runQuery $ retractNote noteId
        redirect "/"
      Nothing -> redirect "/error"
        -- return ref -- Fix with something better, an error msg
    redirect "/"

postNote :: SpockCtxM ctx PG.Connection sess App ()
postNote =
  post root $ do
    author <- param' "author"
    contents <- param' "contents"
    (MkApp ref) <- getState
    newNote <-
      liftIO $ do
        (newNote, updatedAppState) <-
          atomicModifyIORef'
            ref
            (\as ->
               ( snd $ addNoteToAppState author contents as
               , addNoteToAppState author contents as))
        return newNote
    _ <- runQuery $ insertNote newNote
    redirect "/"

addNoteToAppState :: Text -> Text -> AppState -> (Note, AppState)
addNoteToAppState author contents as = (newNote, MkAppState newNotes newNoteId)
  where
    newNote = Note author contents newNoteId
    newNoteId = 1 + (numNotes as)
    newNotes = newNote : (notes as)

removeNote :: Integer -> AppState -> AppState
removeNote i as = as {notes = remainingNotes}
  where
    remainingNotes = filter (\note -> _id note /= i) $ notes as

notesHTML :: [Note] -> Html ()
notesHTML notes' = do
  h1_ "Notes"
  ul_ $ forM_ notes' buildNote

buildNote :: Note -> Html ()
buildNote note =
  li_ $ do
    toHtml (author note)
    ": "
    toHtml (contents note)
    form_ [action_ ("/remove/" <> (pack $ show $ _id note)), method_ "delete" ] $
      button_ [method_ "delete", id_ ((pack . show) (_id note))] "Delete"

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

deleteTableButton_ :: Html ()
deleteTableButton_ =
  form_ [action_ "/deleteAll", method_ "get"] $
    input_ [type_ "submit", value_ "Delete Note Table"]

data App =
  MkApp (IORef AppState)

data AppState = MkAppState
  { notes    :: [Note]
  , numNotes :: Integer
  }

data Note = Note
  { author   :: Text
  , contents :: Text
  , _id      :: Integer
  }

-- Database
deleteNoteTable :: PG.Connection -> IO Int64
deleteNoteTable conn = PG.execute_ conn "DROP TABLE IF EXISTS note"

createNoteTable :: PG.Connection -> IO Int64
createNoteTable conn =
  PG.execute_
    conn
    "CREATE TABLE IF NOT EXISTS note ( \
  \  author varchar(45) NOT NULL, \
  \  contents varchar(1000) NOT NULL, \
  \  _id integer, \
  \  PRIMARY KEY (_id) \
  \  )"

insertNote :: Note -> PG.Connection -> IO Int64
insertNote n conn =
  PG.executeMany
    conn
    "INSERT INTO note (author, contents, _id) VALUES (?, ?, ?)"
    [n]

retractNote :: Integer -> PG.Connection -> IO Int64
retractNote n conn = PG.execute conn "DELETE FROM note where note._id = ?" [n]

allNotes :: PG.Connection -> IO [Note]
allNotes c = PG.query_ c "SELECT author, contents, _id FROM note"

sortedAscNotes :: PG.Connection -> IO [Note]
sortedAscNotes c =
  PG.query_ c "SELECT author, contents, _id FROM note ORDER BY author ASC"

sortedByContent :: PG.Connection -> IO [Note]
sortedByContent c =
  PG.query_ c "SELECT author, contents, _id FROM note ORDER BY contents DESC"

instance PG.ToRow Note where
  toRow n = PG.toField <$> [author n, contents n, pack $ show $ _id n]

instance PG.FromRow Note where
  fromRow = Note <$> PG.field <*> PG.field <*> PG.field

loadConnInfo :: FilePath -> IO PG.ConnectInfo
loadConnInfo path = do
  dbCfg <- C.load $ [C.Required path]
  PG.ConnectInfo <$> C.require dbCfg "host" <*> C.require dbCfg "port" <*>
    C.require dbCfg "user" <*>
    C.require dbCfg "password" <*>
    C.require dbCfg "db"

mkConnBuilder :: PG.ConnectInfo -> ConnBuilder PG.Connection
mkConnBuilder connInfo =
  ConnBuilder
    { cb_createConn = PG.connect connInfo
    , cb_destroyConn = PG.close
    , cb_poolConfiguration =
        PoolCfg {pc_stripes = 1, pc_resPerStripe = 5, pc_keepOpenTime = 60}
    }
