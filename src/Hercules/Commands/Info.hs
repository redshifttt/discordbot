{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Hercules.Commands.Info (cmdInfo) where

import           Hercules.Types
import           Discord.Interactions
import           Discord
import qualified Discord.Requests as R
import Discord.Types
import Data.List (singleton)
import Data.Maybe
import Data.String.Conversions (cs)
import Data.Text (Text)
import Hercules.Errors
import Hercules.Interactions
import Hercules.CommandParameters.Types

cmdInfo :: Command
cmdInfo = Command {
  commandName = "info",
  register = (fromJust $ createChatInput "info" "Show information about users or guilds") {
    createOptions = Just $ OptionsSubcommands [
      OptionSubcommandOrGroupSubcommand $ subcommand {
        optionSubcommandName = "guild",
        optionSubcommandDescription = "Display info about the current guild"
      }
    ]
  },
  handler = handleInfo
}

handleInfo :: Interaction -> DiscordHandler ()
handleInfo int = do
  let cshow :: Show a => a -> Text
      cshow = cs . show

  case interactionGuildId int of
    Nothing -> respond_ int $ interactionResponseBasic "failed to get guild ID"
    Just gldId -> do
      r <- withInteractiveError int $ restCall $ R.GetGuild gldId
      case r of
        Left _ -> return ()
        Right guild ->
          respond_ int $ InteractionResponseChannelMessage (interactionResponseMessageBasic "") {
            interactionResponseMessageEmbeds = Just $ singleton def {
              createEmbedAuthorName = "Guild Info",
              createEmbedTitle = guildName guild,
              createEmbedDescription = cshow $ guildFeatures guild,
              createEmbedFooterText = cshow gldId,
              createEmbedThumbnail = Just $ CreateEmbedImageUrl $ "https://cdn.discordapp.com/icons/" <> cshow gldId <> "/" <> fromJust (guildIcon guild) <> ".png"
            }
          }
