{-# LANGUAGE DerivingVia #-}
module Models where
import GHC.Exts (IsString)
import Args (NumberOfElfs (..))

newtype Elf = Elf String deriving (Eq,Show,IsString) via String
newtype Reindeer = Reindeer String deriving (Eq,Show,IsString) via String

data SantaState = Awake Reason | Sleeping deriving (Eq,Show)

data Reason = Delivering | HelpingElfs deriving (Eq, Show)

data ReindeerState = Vacation | Waiting | AtWork deriving (Eq,Show)

data ElfState = NeedHelp | Busy deriving (Eq,Show)

initElfs:: NumberOfElfs -> [(Elf, ElfState)]
initElfs (NumberOfElfs n) = fmap (elfState . elf) [1..n] where
  elf = Elf . ("Elf " ++ ) . show
  elfState p = (p, Busy)
