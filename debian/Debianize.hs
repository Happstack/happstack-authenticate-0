import Control.Lens
import Data.Set as Set
import Debian.Debianize
import Debian.Relation
import Debian.Debianize.Prelude
import Distribution.Package
import Distribution.PackageDescription

main :: IO ()
main = performDebianization customize
    where
      customize :: CabalT IO ()
      customize = do
        debianDefaults
        -- These belong in cabal-debian/src/Debian/Debianize/Details.hs.  But as of
        -- cabal-debian 4.31.8 repeating them does no harm.
        mapCabal (mkPackageName "happstack-authenticate") (DebBase "happstack-authenticate")
        splitCabal (mkPackageName "happstack-authenticate") (DebBase "happstack-authenticate-0") (mkVersion [2])
        -- (debInfo . overrideDebianNameBase) .= Just (DebBase "happstack-authenticate-0")
        (debInfo . sourceFormat) .= Native3
        (debInfo . flags . cabalFlagAssignments) %= (Set.insert (mkFlagName "migrate", True))
        doExecutable (BinPkgName "happstack-authenticate-migrate")
                     (InstallFile {execName = "happstack-authenticate-migrate",
                                   sourceDir = Nothing, destDir = Nothing,
                                   destName = "happstack-authenticate-migrate"})
        return ()
        -- "--debian-name-base", "happstack-authenticate-0",
        -- "--cabal-flags", "migrate",
        -- "--executable", "happstack-authenticate-migrate"
