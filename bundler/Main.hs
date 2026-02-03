{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.List qualified as L
import Data.Maybe
import Data.Set qualified as S
import Data.Traversable
import Distribution.ModuleName hiding (ModuleName)
import Distribution.PackageDescription hiding (PackageFlag)
import Distribution.Simple.PackageDescription
import Distribution.Utils.Path (makeSymbolicPath)
import Distribution.Verbosity
import GHC.Driver.Session
import GHC.Hs
import GHC.Parser.Lexer
import GHC.Types.SrcLoc
import GHC.Utils.Outputable (renderWithContext, Outputable (ppr))
import Language.Haskell.GhclibParserEx.GHC.Parser(parseFile)
import Language.Haskell.GhclibParserEx.GHC.Settings.Config
import "template-haskell" Language.Haskell.TH(runIO)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process.Extra
import Text.Read

main :: IO ()
main = do

    args <- getArgs

    case args of
        [mode,mainPath] -> do
            unless (isJust (readMaybe @SubmissionMode mode)) $ dieWithInvalidMode mode
            unless (isValid mainPath) $ dieWithInvalidPath mainPath
            execBundle mode mainPath stdout

        [mode,mainPath,destPath] -> do
            unless (isJust (readMaybe @SubmissionMode mode)) $ dieWithInvalidMode mode
            unless (isValid mainPath) $ dieWithInvalidPath mainPath
            unless (isValid destPath) $ dieWithInvalidPath destPath
            withFile destPath WriteMode $ execBundle mode mainPath

        _ -> getProgName >>= dieWithUsage

    where
        execBundle mode mainPath h = do
            pragmas (read mode) mainPath >>= mapM_ (hPutStrLn h)
            bundledMods (read mode) mainPath >>= hPutStrLn h . renderMods

        dieWithInvalidPath path = die $ "Error: '" <> path <> "' is not a valid file path."
        dieWithInvalidMode mode = die $ "Error: '" <> mode <> "' is not a valid SubmissionMode."
        dieWithUsage progName   = die $ "Usage: " <> progName <> " <mode> <mainPath> [destPath]"

newtype BundlerException = BundlerException String
    deriving Show
instance Exception BundlerException

data SubmissionMode = Local | Judge deriving (Eq,Read)
instance Show SubmissionMode where
    show Local = "LOCAL"
    show Judge = "JUDGE"

withPreProcessed :: (MonadThrow m, MonadIO m) => SubmissionMode -> FilePath -> (FilePath -> m a) -> m a
withPreProcessed mode path f = do
    tmpDir <- liftIO getTemporaryDirectory

    let processedFile = tmpDir
                        </> "kyopro_lib_PreProcessed"
                        </> show mode
                        </> case L.stripPrefix installPath path of
                                Just rest -> makeRelative "/" rest
                                Nothing -> throw $ BundlerException "`path` must start with `installPath`"

    liftIO $ createDirectoryIfMissing True (takeDirectory processedFile)

    exists <- liftIO $ doesFileExist processedFile
    if exists
        then withProcessingBasedOnModTime path processedFile f
        else withProcessing path processedFile f

    where
        withProcessingBasedOnModTime :: (MonadThrow m, MonadIO m) => FilePath -> FilePath -> (FilePath -> m a) -> m a
        withProcessingBasedOnModTime modFile cppFile g = do
            modTime <- liftIO $ getModificationTime modFile
            cppTime <- liftIO $ getModificationTime cppFile
            if modTime > cppTime
                then withProcessing modFile cppFile g
                else g cppFile

        withProcessing :: (MonadThrow m, MonadIO m) => FilePath -> FilePath -> (FilePath -> m a) -> m a
        withProcessing modFile cppFile g = do
            exitCode <- liftIO $ rawSystem "cabal" $ [ "exec"
                                                     , "ghc"
                                                     , "--"
                                                     , "-E"
                                                     , modFile
                                                     , "-o"
                                                     , cppFile
                                                     ] <> ["-D" <> show mode | mode == Judge]
            case exitCode of
                ExitSuccess -> g cppFile
                ExitFailure _ -> throw $ BundlerException "Parsing error occured!"

pragmas :: (MonadThrow m, MonadIO m) => SubmissionMode -> FilePath -> m [String]
pragmas mode mainPath = do

    mainExt <- fromFile $ installPath </> mainPath
    reachables <- reachableKyoproMods mode (installPath </> mainPath)
    libExts <- fmap concat <$> for reachables $ fromFile . modNameToPath (installPath </> "src/")

    return $ S.toList $ S.fromList $ mainExt <> libExts

    where fromFile :: (MonadThrow m, MonadIO m) => FilePath -> m [String]
          fromFile path = do
              content <- lines <$> liftIO (readFile' path)
              return $ filter (\x -> ("LANGUAGE" `L.isInfixOf` x || "OPTIONS_GHC" `L.isInfixOf` x) && not ("CPP" `L.isInfixOf` x)) content

bundledMods :: (MonadThrow m, MonadIO m) => SubmissionMode -> FilePath -> m (HsModule GhcPs)
bundledMods mode mainPath = do
    mainMod <- parseHs mode (installPath </> mainPath)
    reachables <- reachableKyoproMods mode (installPath </> mainPath)
    libMods <- for reachables $ parseHs mode . modNameToPath (installPath </> "src/")

    pure $ foldr rmIDecl (mainMod { hsmodDecls = hsmodDecls mainMod <> foldMap hsmodDecls libMods
                                  , hsmodImports = nubIDecls (hsmodImports mainMod <> foldMap hsmodImports libMods)
                                  }) reachables

renderMods :: HsModule GhcPs -> String
renderMods = renderWithContext (initDefaultSDocContext dynFlags) . ppr

renderErrors :: PState -> String
renderErrors = renderWithContext (initDefaultSDocContext dynFlags) . ppr . getPsErrorMessages

importedModuleName :: HsModule GhcPs -> [String]
importedModuleName hsMod = [ moduleNameString . unLoc . ideclName . unLoc $ imp | imp <- hsmodImports hsMod]

reachableKyoproMods :: (MonadThrow m, MonadIO m) => SubmissionMode -> FilePath -> m [String]
reachableKyoproMods mode mainPath = do
    kyoproSet <- S.fromList <$> kyoproExposedMods
    mainHs <- parseHs mode (installPath </> mainPath)

    let modsInMain = filter (`S.member`kyoproSet) $ importedModuleName mainHs

        go seen m = if m `S.member` seen then return seen  else do
            hs <- parseHs mode (modNameToPath (installPath </> "src/") m)
            let cands = filter (`S.member`kyoproSet) $ importedModuleName hs
            foldlM go (S.insert m seen) cands

    S.toList <$> foldlM go S.empty modsInMain

parseHs :: (MonadThrow m, MonadIO m) => SubmissionMode -> FilePath -> m (HsModule GhcPs)
parseHs mode path = parse path =<< withPreProcessed mode path (liftIO . readFile')

kyoproExposedMods :: (MonadThrow m, MonadIO m) => m [String]
kyoproExposedMods = do
    let cabalFile = makeSymbolicPath (installPath </> "kyopro.cabal")

    gpd <- liftIO $ readGenericPackageDescription normal Nothing cabalFile
    case condLibrary gpd of
        Nothing -> return []
        Just (CondNode libs _ _) -> return $ map (L.intercalate "." . components) $ exposedModules libs

modNameToPath :: FilePath -> String -> FilePath
modNameToPath basePath modName = basePath </> map (\x -> if x == '.' then '/' else x) modName <.> "hs"

rmIDecl :: String -> HsModule GhcPs -> HsModule GhcPs
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

parse :: MonadThrow m => FilePath -> String -> m (HsModule GhcPs)
parse filename code =
    case parseFile filename dynFlags code of
        POk _ located -> return $ unLoc located
        PFailed ps -> throw $ BundlerException $ renderErrors ps

installPath :: FilePath
installPath = $(do dir <- runIO getCurrentDirectory; [e|dir|])

dynFlags :: DynFlags
dynFlags = defaultDynFlags fakeSettings
