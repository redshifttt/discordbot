module Hercules.Types where

import Universum
import Discord.Interactions
import Discord

data Command = Command {
  commandName :: Text,
  register :: CreateApplicationCommand,
  handler :: Interaction -> DiscordHandler ()
}
