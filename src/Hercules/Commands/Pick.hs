{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Hercules.Commands.Pick (cmdPick) where

import           Hercules.Types
import           Discord.Interactions
import           Discord
import qualified Discord.Requests as R
import           Control.Monad
import           System.Random
import Data.Maybe
import Hercules.Errors
import Hercules.CommandParameters.Types
import Hercules.Interactions

cmdPick :: Command
cmdPick = Command {
  commandName = "pick",
  register = (fromJust $ createChatInput "pick" "Pick a random option") {
    createOptions = Just $ OptionsValues [
      string {
        optionValueName = "first",
        optionValueDescription = "The first option"
      },
      string {
        optionValueName = "second",
        optionValueDescription = "The second option"
      }
    ]
  },
  handler = handlePick
}

handlePick :: Interaction -> DiscordHandler ()
handlePick int = do
  let opts = fromJust $ optionsData $ applicationCommandData int

  randomIndex <- randomIO
  case opts of
    OptionsDataValues optsValues -> do
      case optionDataValueString $ optsValues !! (randomIndex `mod` 2) of
        -- there appears to be no documentation in discord-haskell as to what the Left case is supposed to do
        Left text -> respond_ int $ interactionResponseBasic $ text <> " (this is the mysterious Left case that max has been searching for)"
        Right text -> respond_ int $ interactionResponseBasic text
    OptionsDataSubcommands _ -> return ()
