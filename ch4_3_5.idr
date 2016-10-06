module Main

import Data.Vect

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newitem]
    addToData (x :: xs) = x :: addToData xs

data Command = Add String
             | Get Integer
             | Size
             | Search String
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand cmd args = case cmd of
                             "add" => Just(Add args)
                             "get" => if all isDigit (unpack args) then
                                        Just (Get (cast args))
                                      else
                                        Nothing
                             "size" => Just Size
                             "quit" => Just Quit
                             "search" => Just (Search args)
                             _ => Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                     case integerToFin pos (size store) of
                     Nothing => Just ("Out of range\n", store)
                     Just id => Just (index id store_items ++ "\n", store)

findItem : (string : String) -> (store_items : Vect size String) -> String
findItem string store_items = findItem0 0 store_items where
  findItem0 : Int -> (store_items : Vect size String) -> String
  findItem0 _ [] = ""
  findItem0 pos (item :: items) = if Strings.isInfixOf string item then
                                     show(pos) ++ ": " ++ item ++ "\n" ++ (findItem0 (pos + 1) items)
                                  else
                                     findItem0 (pos + 1) items

search : (string : String) -> (store : DataStore) -> String
search string store = let store_items = items store in
                      findItem string store_items

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse inp of
                          Nothing => Just ("Invalid command\n", store)
                          Just (Add item) =>
                            Just(("ID " ++ show (size store) ++ "\n"), (addToStore store item))
                          Just Size =>
                            Just(("ID " ++ show (size store) ++ "\n"), store)
                          Just (Get pos) => getEntry pos store
                          Just (Search item) =>
                            Just(((search item store) ++ "\n"), store)
                          Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
