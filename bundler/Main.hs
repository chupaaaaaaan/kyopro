{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad.IO.Class
import Data.List (intercalate, isInfixOf, nub)
import Data.Set qualified as S
import Distribution.ModuleName hiding (ModuleName)
import Distribution.PackageDescription hiding (PackageFlag)
import Distribution.Simple.PackageDescription
import Distribution.Verbosity
import GHC.Driver.Session
import GHC.Hs
import GHC.Parser.Lexer
import GHC.Types.SrcLoc
import GHC.Unit.Module.Name
import GHC.Utils.Outputable (renderWithContext, Outputable (ppr))
import Language.Haskell.GhclibParserEx.GHC.Parser(parseFile)
import Language.Haskell.GhclibParserEx.GHC.Settings.Config
import "template-haskell" Language.Haskell.TH(runIO)
import System.Directory
import System.Environment
import System.FilePath
import System.IO

main :: IO ()
main = do

    args <- getArgs

    case args of
        [] -> error "Must be 1 or 2 args"

        [mainPath] -> do
            pragmas mainPath >>= mapM_ putStrLn
            bundledMods mainPath >>= putStrLn . renderMods

        (mainPath:destPath:_) -> do
            withFile destPath WriteMode $ \h -> do
                pragmas mainPath >>= mapM_ (hPutStrLn h)
                bundledMods mainPath >>= hPutStrLn h . renderMods

    return ()

astFromFile :: FilePath -> IO HsModule
astFromFile path = parse path <$> readFile' path

pragmaFromFile :: FilePath -> IO [String]
pragmaFromFile path = do
    content <- lines <$> readFile' path
    return $ filter (\x -> "LANGUAGE" `isInfixOf` x) content

pragmas :: FilePath -> IO [String]
pragmas mainPath = do

    mainExt <- pragmaFromFile mainPath
    libExts <- fmap concat $ mapM (pragmaFromFile . modNameToPath (installPath </> "src/")) =<< kyoproExposedMods

    return (nub $ mainExt <> libExts)

bundledMods :: FilePath -> IO HsModule
bundledMods mainPath = do

    (mainMod, libMods) <- targetMods mainPath
    
    foldr rmIDecl (mainMod { hsmodDecls = hsmodDecls mainMod <> foldMap hsmodDecls libMods
                           , hsmodImports = nubIDecls (hsmodImports mainMod <> foldMap hsmodImports libMods)
                           }) <$> kyoproExposedMods

renderMods :: HsModule -> String
renderMods hsMod = let ctx = initDefaultSDocContext dynFlags
                   in renderWithContext ctx $ ppr hsMod


targetMods :: FilePath -> IO (HsModule, [HsModule])
targetMods mainPath = do
    
    mainMod <- astFromFile mainPath
    libMods <- mapM (astFromFile . modNameToPath (installPath </> "src/")) =<< kyoproExposedMods

    return (mainMod, libMods)

kyoproExposedMods :: IO [String]
kyoproExposedMods = do
    let cabalFile = installPath </> "kyopro.cabal"

    gpd <- liftIO $ readGenericPackageDescription normal cabalFile
    case condLibrary gpd of
        Nothing -> return []
        Just (CondNode libs _ _) -> return $ map (intercalate "." . components) $ exposedModules libs

modNameToPath :: FilePath -> String -> FilePath
modNameToPath basePath modName = basePath </> map (\x -> if x == '.' then '/' else x) modName <.> "hs"

rmIDecl :: String -> HsModule -> HsModule
rmIDecl name hsMod = let ilist = filter ((/=name) . moduleNameString . unLoc . ideclName . unLoc) . hsmodImports $ hsMod
                     in hsMod {hsmodImports = ilist}

nubIDecls :: [LImportDecl GhcPs] -> [LImportDecl GhcPs]
nubIDecls = snd . foldr (\idecl (set, acc) ->
                                let nm = normIDecl idecl
                                in if nm `S.member` set then (set, acc) else (nm `S.insert` set, idecl:acc)) (S.empty, [])
            where normIDecl :: LImportDecl GhcPs -> (String, Bool, Maybe String)
                  normIDecl (L _ impDecl) =
                    let modName = moduleNameString . unLoc . ideclName $ impDecl
                        qualifiedFlag = case ideclQualified impDecl of
                                            NotQualified -> False
                                            QualifiedPre -> True
                                            QualifiedPost -> True
                        alias = moduleNameString . unLoc <$> ideclAs impDecl
                    in (modName, qualifiedFlag, alias)

parse :: FilePath -> String -> HsModule
parse filename code = 
    case parseFile filename dynFlags code of
        POk _ (L _ hsMod) -> hsMod
        PFailed _ -> error "Parsing failed"

installPath :: FilePath
installPath = $(do dir <- runIO getCurrentDirectory; [e|dir|])

dynFlags :: DynFlags
dynFlags = defaultDynFlags fakeSettings fakeLlvmConfig
-- dynFlags = foldl xopt_set (defaultDynFlags fakeSettings fakeLlvmConfig) exts

-- exts :: [Extension]
-- exts =
--   [ X.FunctionalDependencies
--   , X.DerivingStrategies
--   , X.UndecidableInstances
--   ]
