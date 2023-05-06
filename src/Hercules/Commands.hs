module Hercules.Commands where

import           Control.Monad.IO.Class
import           Control.Monad
import qualified Data.Map as M
import           Data.Text (Text)

import           Discord.Interactions
import           Discord
import           Discord.Types
import qualified Discord.Requests as R

import           Hercules.Types
import qualified Hercules.Commands.Info
import qualified Hercules.Commands.Pick

allCommands :: M.Map Text Command
allCommands = M.fromList $ map (\cmd -> (commandName cmd, cmd)) [
    Hercules.Commands.Info.cmdInfo,
    Hercules.Commands.Pick.cmdPick
  ]

registerCommands :: PartialApplication -> DiscordHandler ()
registerCommands pApp = do
  let appId = partialApplicationID pApp
      registerCmd c = do
        err <- restCall (R.CreateGlobalApplicationCommand appId c)
        case err of
          Left e -> liftIO $ print e
          _ -> return ()
  mapM_ (registerCmd . register) (M.elems allCommands)

handleInteraction :: Interaction -> DiscordHandler ()
handleInteraction interaction = do

  liftIO $ putStrLn $ "Interaction received: " ++ show interaction

  case M.lookup (applicationCommandDataName $ applicationCommandData interaction) allCommands of
    Just cmd -> handler cmd interaction
    Nothing -> let
        intId = interactionId interaction
        tok = interactionToken interaction
      in void $ restCall $ R.CreateInteractionResponse intId tok $ interactionResponseBasic "unknown command"
