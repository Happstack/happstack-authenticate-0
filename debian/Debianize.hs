import Control.Lens
import Data.Set as Set
import Debian.Debianize
import Debian.Relation
import Distribution.Package (PackageName(PackageName))
import Distribution.PackageDescription (FlagName(FlagName))
import Data.Version (Version(Version))

main :: IO ()
main = performDebianization customize
    where
      customize :: CabalT IO ()
      customize = do
        debianDefaults
        -- These belong in cabal-debian/src/Debian/Debianize/Details.hs.  But as of
        -- cabal-debian 4.31.8 repeating them does no harm.
        mapCabal (PackageName "happstack-authenticate") (DebBase "happstack-authenticate")
        splitCabal (PackageName "happstack-authenticate") (DebBase "happstack-authenticate-0") (Version [2] [])
        -- (debInfo . overrideDebianNameBase) .= Just (DebBase "happstack-authenticate-0")
        (debInfo . sourceFormat) .= Native3
        (debInfo . flags . cabalFlagAssignments) %= (Set.insert (FlagName "migrate", True))
        doExecutable (BinPkgName "happstack-authenticate-migrate")
                     (InstallFile {execName = "happstack-authenticate-migrate",
                                   sourceDir = Nothing, destDir = Nothing,
                                   destName = "happstack-authenticate-migrate"})
        return ()
        -- "--debian-name-base", "happstack-authenticate-0",
        -- "--cabal-flags", "migrate",
        -- "--executable", "happstack-authenticate-migrate"
