module Mainframe.Widgets.Embeds where

import Universum
import Discord
import Discord.Types

errorEmbed :: RestCallErrorCode -> CreateEmbed
errorEmbed (RestCallErrorCode errCode errTitle errBody) = 
  def {
    createEmbedAuthorName = "REST Call Error",
    createEmbedTitle = "HTTP " <> show errCode <> " " <> errTitle,
    createEmbedDescription = errBody,
    createEmbedColor = Just DiscordColorRed
  }
