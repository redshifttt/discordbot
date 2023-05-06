module Hercules.Errors where

import Discord
import Discord.Types
import Data.Text (Text)
import Data.String.Conversions (cs)
import Discord.Interactions
import qualified Discord.Requests as R
import Data.List (singleton)
import Control.Monad

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

withInteractiveError :: Interaction -> DiscordHandler (Either RestCallErrorCode a) -> DiscordHandler (Either RestCallErrorCode a)
withInteractiveError int action = do
  let intId = interactionId int
      tok = interactionToken int
  res <- action
  case res of
    Left err -> do
      let  resp = (interactionResponseMessageBasic "") {
            interactionResponseMessageEmbeds = Just $ singleton $ errorEmbed err
          }
      void $ restCall (R.CreateInteractionResponse intId tok $ InteractionResponseChannelMessage resp)
      return res
    Right _ -> return res
  