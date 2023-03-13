module Sound.Tidal.Listener.Command where

import Options.Applicative
import Sound.Tidal.Listener.Config

conf :: ParserInfo Config
conf = info (configParser <**> helper)
  ( fullDesc
  <> progDesc "An OSC interpreter for TidalCycles"
  <> header "tidal-listener" )

configParser :: Parser Config
configParser = Config <$> listenPortParser
                      <*> replyPortParser
                      <*> dirtPortParser
                      <*> noGhcParser

listenPortParser :: Parser Int
listenPortParser = option auto
                      ( long "listenport"
                     <> short 'l'
                     <> help "Specify the listening port"
                     <> showDefault
                     <> value 6011
                     <> metavar "INT" )

replyPortParser :: Parser Int
replyPortParser = option auto
                     ( long "replyport"
                    <> short 'r'
                    <> help "Specify the reply port"
                    <> showDefault
                    <> value 6012
                    <> metavar "INT")

dirtPortParser :: Parser Int
dirtPortParser = option auto
                     ( long "dirtport"
                    <> short 'd'
                    <> help "Specify the dirt port"
                    <> showDefault
                    <> value 5720
                    <> metavar "INT")

noGhcParser :: Parser Bool
noGhcParser =  switch
          ( long "no-ghc"
         <> help "If this flag is active, the interpreter will assume that GHC not installed on the system" )
