{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Hercules.Commands.Info (cmdInfo) where

import           Hercules.Types
import           Discord.Interactions
import           Discord
import qualified Discord.Requests as R
import           Control.Monad
import Discord.Types
import Data.List (singleton)
import Data.Maybe
import Data.String.Conversions (cs)
import Data.Text (Text)
import Hercules.Errors

cmdInfo :: Command
cmdInfo = Command {
  commandName = "info",
  register = (fromJust $ createChatInput "info" "Show information about users or guilds") {
    createOptions = Nothing
  },
  handler = handleInfo
}

handleInfo :: Interaction -> DiscordHandler ()
handleInfo int = do
  let intId = interactionId int
      tok = interactionToken int
      cshow :: Show a => a -> Text
      cshow = cs . show

  case interactionGuildId int of
    Nothing -> void $ restCall $ R.CreateInteractionResponse intId tok $ interactionResponseBasic "failed to get guild ID"
    Just gldId -> do
      r <- withInteractiveError int $ restCall $ R.GetGuild gldId
      case r of
        Left _ -> return ()
        Right guild -> do
          let  resp = (interactionResponseMessageBasic "") {
                interactionResponseMessageEmbeds = Just $ singleton def {
                  createEmbedAuthorName = "Guild Info",
                  createEmbedTitle = guildName guild,
                  createEmbedDescription = cshow $ guildFeatures guild,
                  createEmbedFooterText = cshow gldId,
                  createEmbedThumbnail = Just $ CreateEmbedImageUrl $ "https://cdn.discordapp.com/icons/" <> cshow gldId <> "/" <> fromJust (guildIcon guild) <> ".png"
                }
              }
          void $ withInteractiveError int $ restCall $ R.CreateInteractionResponse intId tok $ InteractionResponseChannelMessage resp
