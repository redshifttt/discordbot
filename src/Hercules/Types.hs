module Hercules.Types where

import Data.Text
import Discord.Interactions
import Discord

data Command = Command {
  commandName :: Text,
  register :: CreateApplicationCommand,
  handler :: Interaction -> DiscordHandler ()
}
