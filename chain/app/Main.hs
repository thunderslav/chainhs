{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Main where

import qualified Control.Monad.State as S
import           Data.Aeson          hiding (json)
import qualified Data.ByteString
import           Data.IORef
import           Data.Monoid         ((<>))
import           Data.String         (fromString)
import           Data.Text           (Text, pack)
import           GHC.Generics
import           Lib

newtype RunStateT s m = RunStateT { runStateT :: forall a. S.StateT s m a -> m a }

type Blockchain = [Block]

main :: IO ()
main = return ()

-- add transaction

addTx :: (Monad m) => Transaction -> S.StateT Blockchain m Blockchain
addTx tx = do 
    bc <- S.get
    let bh = Prelude.head bc --block head
    let bt = Prelude.tail bc --block tail
    let bh' = blockAddTx bh tx --adds transaction to head of block
    let bc' = ([bh'] ++ bt) -- defining blockchain as, block header, mied block and block tail
    S.put bc'
    return [bh'] --return mined block

-- mining block head

mine :: (Monad m) => S.StateT Blockchain m Blockchain
mine = do
    bc <- S.get -- Blockchain
    let bh = Prelude.head bc -- Block head
    let bt = Prelude.tail bc -- Block tail
    let mb = mineBlock bh 0 -- Mined block
    let bh' = Block (index mb) [] "" (hash mb) Nothing -- New block head
    let bc' = ([bh', mb] ++ bt)
    S.put bc'
    return [mb]

--returning of current blockhead

current :: (Monad m) => S.StateT Blockchain m Blockchain
current = do
    bc <- S.get
    return $ [Prelude.head bc]

-- getting current hash
getBlockhash :: (Monad m) => String -> S.StateT Blockchain m Blockchain
getBlockhash h = do
    bc <- S.get
    return $ filter (\b -> (hash b) == h) bc


-- gets entire blockchain

getBlockchain :: (Monad m) => S.StateT Blockchain m Blockchain
getBlockchain = S.get >>= return

--State Monad to wroap the blockchain around, miking it 'mutable'