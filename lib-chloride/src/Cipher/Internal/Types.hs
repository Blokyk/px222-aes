module Cipher.Internal.Types where

import Prelude hiding (Word)

import Word

type Column = Word
type Key = [Word]
type Block = [Column]