module Hercules.Interactions where

import Control.Monad
import Control.Monad.Trans.Reader

import Discord
import Discord.Interactions
import qualified Discord.Requests as R

import Hercules.Errors

respond :: Interaction -> InteractionResponse -> DiscordHandler (Either RestCallErrorCode ())
respond int resp = let
    intId = interactionId int
    tok = interactionToken int
  in withInteractiveError int $ restCall $ R.CreateInteractionResponse intId tok resp

respond_ :: Interaction -> InteractionResponse -> ReaderT DiscordHandle IO ()
respond_ int resp = void $ respond int resp
