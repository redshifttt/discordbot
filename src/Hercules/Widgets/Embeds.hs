module Hercules.Widgets.Embeds where

import Data.Text
import Data.String.Conversions (cs)
import Discord
import Discord.Types

errorEmbed :: RestCallErrorCode -> CreateEmbed
errorEmbed (RestCallErrorCode errCode errTitle errBody) = let
  cshow :: Show a => a -> Text
  cshow = cs . show
  in def {
    createEmbedAuthorName = "REST Call Error",
    createEmbedTitle = "HTTP " <> cshow errCode <> " " <> errTitle,
    createEmbedDescription = errBody,
    createEmbedColor = Just DiscordColorRed
  }
