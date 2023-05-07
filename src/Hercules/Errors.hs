module Hercules.Errors where

import Data.List (singleton)
import Control.Monad

import Discord
import Discord.Interactions
import qualified Discord.Requests as R

import Hercules.Widgets.Embeds

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
  