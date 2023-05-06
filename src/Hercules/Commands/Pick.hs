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

cmdPick :: Command
cmdPick = Command {
  commandName = "pick",
  register = (fromJust $ createChatInput "pick" "Pick a random option") {
    createOptions = Just $ OptionsValues [
      OptionValueString {
        optionValueName = "first",
        optionValueLocalizedName = Nothing,
        optionValueDescription = "The first option",
        optionValueLocalizedDescription = Nothing,
        optionValueStringChoices = Left False,
        optionValueRequired = True,
        optionValueStringMinLen = Nothing,
        optionValueStringMaxLen = Nothing
      },
      OptionValueString {
        optionValueName = "second",
        optionValueLocalizedName = Nothing,
        optionValueDescription = "The second option",
        optionValueLocalizedDescription = Nothing,
        optionValueStringChoices = Left False,
        optionValueRequired = True,
        optionValueStringMinLen = Nothing,
        optionValueStringMaxLen = Nothing
      }
    ]
  },
  handler = handlePick
}

handlePick :: Interaction -> DiscordHandler ()
handlePick int = do
  let intId = interactionId int
      tok = interactionToken int
      opts = fromJust $ optionsData $ applicationCommandData int

  randomIndex <- randomIO
  case opts of
    OptionsDataValues optsValues -> do
      case optionDataValueString $ optsValues !! (randomIndex `mod` 2) of
        Left text -> void $ withInteractiveError int $ restCall $ R.CreateInteractionResponse intId tok $ interactionResponseBasic text
        Right text -> void $ withInteractiveError int $ restCall $ R.CreateInteractionResponse intId tok $ interactionResponseBasic text
    OptionsDataSubcommands _ -> return ()
