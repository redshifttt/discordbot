module Mainframe.Commands where

import           Universum
import qualified Data.Map as M

import           Discord.Interactions
import           Discord
import           Discord.Types
import qualified Discord.Requests as R

import           Mainframe.Types
import qualified Mainframe.Commands.Info
import qualified Mainframe.Commands.Pick
import           Mainframe.Interactions

allCommands :: M.Map Text Command
allCommands = M.fromList $ map (\cmd -> (commandName cmd, cmd)) [
    Mainframe.Commands.Info.cmdInfo,
    Mainframe.Commands.Pick.cmdPick
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
    Nothing -> respond_ interaction $ interactionResponseBasic "unknown command"
