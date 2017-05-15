{-# LANGUAGE OverloadedStrings #-}
module Storage where

import Database.SQLite.Simple as Sql
import Database.SQLite.Simple.Types as SqlTypes

import qualified Model as M
import qualified Data.Text as Txt


instance Sql.FromRow M.NewUser where
  fromRow = M.NewUser <$> Sql.field <*> Sql.field  <*> Sql.field  <*> Sql.field


createTables :: Sql.Connection -> IO ()
createTables conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS Users (id INTEGER PRIMARY KEY, ..."


 {-
newUser :: Sql.Connection -> NewUser -> IO (UserID)
newUser conn user = do
  Sql.execute conn "insert into users ("

newArtist :: Sql.Connection -> M.Artist -> IO M.Artist
newArtist conn artist = do
  Sql.execute conn "insert into artist (name) values (?) " (Sql.Only $ M.artistName artist)
  rawId <- lastInsertRowId conn
  let updArtist = artist { M.artistId = Just (fromIntegral rawId) }
  return updArtist










artistById :: Sql.Connection -> Int -> IO (Maybe M.Artist)
artistById conn idParam =
  findById conn "artist" idParam :: IO (Maybe M.Artist)


findArtists :: Sql.Connection -> IO [M.Artist]
findArtists conn =
  Sql.query_ conn "select * from artist" :: IO [M.Artist]


newArtist :: Sql.Connection -> M.Artist -> IO M.Artist
newArtist conn artist = do
  Sql.execute conn "insert into artist (name) values (?) " (Sql.Only $ M.artistName artist)
  rawId <- lastInsertRowId conn
  let updArtist = artist { M.artistId = Just (fromIntegral rawId) }
  return updArtist


-- Really we should check whether the artist exists here
updateArtist :: Sql.Connection -> M.Artist -> Int -> IO M.Artist
updateArtist conn artist idParam = do
  Sql.executeNamed conn "update artist set name = :name where id = :id" params
  return artist { M.artistId = Just idParam }
  where
    params = [":id" := (idParam :: Int), ":name" := ((M.artistName artist) :: String)]


deleteArtist :: Sql.Connection -> Int -> IO ()
deleteArtist conn idParam =
  Sql.execute conn "delete from artist where id = ?" (Sql.Only idParam)


findById :: (FromRow a) => Sql.Connection -> String -> Int -> IO (Maybe a)
findById conn table idParam = do
  rows <- Sql.queryNamed conn (createFindByIdQuery table) [":id" := (idParam :: Int)]
  let result = case (length rows) of
                  0 -> Nothing
                  _ -> Just $ head rows

  return result


createFindByIdQuery :: String -> SqlTypes.Query
createFindByIdQuery table =
  SqlTypes.Query $ Txt.pack $ "SELECT * from " ++ table ++ " where id = :id"

-- ... boostrap function left out, check the source repo for details
--
-}
