module Main where

import Language.Cobble.Prelude hiding (argument)
import Language.Cobble
import Language.Cobble.Types
import Language.Cobble.Packager
import Language.Cobble.Errors

import Options.Applicative
import Language.Cobble.Util.Polysemy.Time

main :: IO ()
main = runCobble =<< execParser (info (mainOpts <**> helper) mainInfo)
    where
        mainOpts = hsubparser (
              command "compile" (Compile <$> info compileOpts (progDesc "Compile a source fileto a datapack directly"))
            ) 
        mainInfo = idm

cobbleInfo :: InfoMod CobbleAction
cobbleInfo = mempty

runCobble :: CobbleAction -> IO ()
runCobble = \case
    Compile co -> runCompile co

runCompile :: CompileCmdOpts -> IO ()
runCompile CompileCmdOpts{compFiles, debug, packageName, description, logLevel, ddumpLC, ddumpCPS, ddumpReduced, ddumpTL, ddumpAsm} = do
    let opts = CompileOpts {
            name=packageName
        ,   debug
        ,   description
        ,   ddumpLC
        ,   ddumpCPS
        ,   ddumpReduced
        ,   ddumpTL    
        ,   ddumpAsm 
        }
    (logs, edatapackBS) <- runControllerC opts (timeToIO $ compileToDataPack compFiles)
    traverse_ (printLog logLevel) logs
    case edatapackBS of
        Left e -> prettyPrintError e
        Right datapackBS -> writeFileLBS (toString packageName <> ".zip") datapackBS

printLog :: LogLevel -> Log -> IO ()
printLog maxLevel (Log lvl o) = when (lvl <= maxLevel)
    $ putTextLn (logPrefix lvl <> o <> "\ESC[0m")
    where
        logPrefix = \case
            LogWarning -> "\ESC[38;2;255;128;0m\STX[WARNING] "
            LogInfo    -> "\ESC[38;2;0;225;225m\STX[INFO] "
            LogVerbose -> "\ESC[38;2;0;128;128m\STX[VERBOSE] "
            LogDebug -> "\ESC[38;2;0;225;0m\STX[DEBUG] "
            LogDebugVerbose -> "\ESC[38;2;0;128;0m\STX[DEBUG VERBOSE] "
            LogDebugVeryVerbose -> "\ESC[38;2;0;100;0m\STX[DEBUG VERY VERBOSE] "


data CobbleAction = Compile CompileCmdOpts deriving (Show, Eq)

data CompileCmdOpts = CompileCmdOpts {
      compFiles :: [FilePath]
    , packageName :: Text
    , debug :: Bool
    , logLevel :: LogLevel
    , description :: Text
    , ddumpLC :: Bool
    , ddumpCPS :: Bool
    , ddumpReduced :: Bool
    , ddumpTL :: Bool
    , ddumpAsm :: Bool
    } deriving (Show, Eq)

compileOpts :: Parser CompileCmdOpts
compileOpts = CompileCmdOpts
    <$> some (argument str (metavar "SOURCEFILES"))
    <*> strOption (long "package-name" <> short 'p' <> metavar "NAME" <> help "The name of the package/datapack")
    <*> switch (long "debug" <> help "Debug mode keeps additional information at runtime")
    <*> option auto (long "log-level" <> metavar "LEVEL" <> value LogInfo <> help "Controls how much information is logged (LogWarning | LogInfo | LogVerbose | LogDebug | LogDebugVerbose)")
    <*> strOption (long "description" <> short 'd' <> metavar "DESCRIPTION" <> help "The datapack description for the datapack's pack.mcmeta")
    <*> switch (long "ddump-lc" <> help "Write the intermediate lambda calculus to a file")
    <*> switch (long "ddump-cps" <> help "Write the intermediate CPS to a file")
    <*> switch (long "ddump-reduced" <> help "Write the reduced intermediate CPS to a file")
    <*> switch (long "ddump-tl" <> help "Write the intermediate TopLevel CPS to a file")
    <*> switch (long "ddump-asm" <> help "Write the intermediate ASM to a file")
