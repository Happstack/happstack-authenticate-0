import Control.Lens
import Data.Set as Set
import Debian.Debianize
import Debian.Relation
import Distribution.PackageDescription (FlagName(FlagName))

main :: IO ()
main = performDebianization customize
    where
      customize :: CabalT IO ()
      customize = do
        debianDefaults
        (debInfo . sourceFormat) .= Native3
        (debInfo . overrideDebianNameBase) .= Just (DebBase "happstack-authenticate-0")
        (debInfo . flags . cabalFlagAssignments) %= (Set.insert (FlagName "migrate", True))
        doExecutable (BinPkgName "happstack-authenticate-migrate")
                     (InstallFile {execName = "happstack-authenticate-migrate",
                                   sourceDir = Nothing, destDir = Nothing,
                                   destName = "happstack-authenticate-migrate"})
        return ()
        -- "--debian-name-base", "happstack-authenticate-0",
        -- "--cabal-flags", "migrate",
        -- "--executable", "happstack-authenticate-migrate"
