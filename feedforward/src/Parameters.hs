module Parameters where

import Options.Applicative

data Parameters = Parameters {scripts :: [String], 
                              historyFile :: Maybe String, 
                              historyOffset :: Maybe Double,
                              historyLoop :: Bool
                              }

parameters :: Parser Parameters
parameters = Parameters
      <$> many ( strOption
                 ( long "scripts"
                   <> short 's'
                   <> metavar "FILE"
                   <> help "Script file to be executed at start"
                 )
               )
      <*> optional (strOption $ long "history-file"
            <> help "History file for replay"
          )
      <*> optional (option auto $ long "history-offset"
            <> help "Time offset for history replay"
          )
      <*> switch (long "history-loop" 
                  <> short 'l' 
                  <> help "Whether to loop as the end of a history file."
                  )
