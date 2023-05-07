{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Hercules.Commands.Info (cmdInfo) where

import           Universum
import           Hercules.Types
import           Discord.Interactions
import           Discord
import qualified Discord.Requests as R
import Discord.Types
import Data.List (singleton)
import Data.Maybe
import Hercules.Interactions
import Hercules.CommandParameters.Types
import qualified Data.List as L

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
  case interactionGuildId int of
    Nothing -> respond_ int $ interactionResponseBasic "failed to get guild ID"
    Just gldId -> runInteraction int $ do
      r <- restCall $ R.GetGuild gldId
      return $ r >>= \guild ->
        return $ respond int $ InteractionResponseChannelMessage (interactionResponseMessageBasic "") {
          interactionResponseMessageEmbeds = let
            simpleCounter name valuesF = Just EmbedField {
              embedFieldName = name,
              embedFieldInline = Just False,
              embedFieldValue = show $ L.length $ valuesF guild
            }
          in Just $ singleton def {
            createEmbedAuthorName = "Guild Info",
            createEmbedTitle = guildName guild,
            createEmbedFields = catMaybes [
              guildMembers guild >>= \members -> Just EmbedField {
                embedFieldName = "Members",
                embedFieldInline = Just False,
                embedFieldValue = let
                  memberCount = length members

                  memberIsRegular member = fromMaybe False $ memberUser member >>= \user -> Just $ not $ userIsBot user

                  memberCountRegular = length $ filter memberIsRegular members

                in show memberCount <> " (" <> show memberCountRegular <> " users, " <> show (memberCount - memberCountRegular) <> " bots)"
              },
              simpleCounter "Emojis" guildEmojis,
              simpleCounter "Roles" guildRoles,
              simpleCounter "Stickers" guildStickers
            ],
            createEmbedDescription = show $ guildFeatures guild,
            createEmbedFooterText = show gldId,
            createEmbedThumbnail = guildIcon guild >>= \icon -> Just $ CreateEmbedImageUrl $ "https://cdn.discordapp.com/icons/" <> show gldId <> "/" <> icon <> ".png"
          }
        }
