module Mainframe.Interactions where

import Universum

import Discord
import Discord.Interactions
import qualified Discord.Requests as R

import Mainframe.Errors

respond :: Interaction -> InteractionResponse -> DiscordHandler (Either RestCallErrorCode ())
respond int resp = let
    intId = interactionId int
    tok = interactionToken int
  in withInteractiveError int $ restCall $ R.CreateInteractionResponse intId tok resp

respond_ :: Interaction -> InteractionResponse -> DiscordHandler ()
respond_ int resp = void $ respond int resp

runInteraction :: Interaction -> DiscordHandler (Either RestCallErrorCode a) -> DiscordHandler ()
runInteraction int action = void $ withInteractiveError int action
