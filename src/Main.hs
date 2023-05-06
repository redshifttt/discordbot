import           Control.Monad (when, void)
import           Data.Text (isPrefixOf, toLower)
import qualified Data.Text.IO as TIO
import           System.Environment (getEnv)
import           Data.String.Conversions (cs)
import           Control.Monad.IO.Class

import           Discord
import           Discord.Types
import qualified Discord.Requests as R

import           Hercules.Commands

-- | Replies "pong" to every message that starts with "ping"
main :: IO ()
main = do
    token <- getEnv "HERCULES_BOT_TOKEN"
    userFacingError <- runDiscord $ def
             { discordToken = cs token
             , discordOnEvent = eventHandler
             , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
             } -- if you see OnLog error, post in the discord / open an issue

    TIO.putStrLn userFacingError
    -- userFacingError is an unrecoverable error
    -- put normal 'cleanup' code in discordOnEnd (see examples)

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
    MessageCreate m -> do
        liftIO $ putStrLn $ "Message received: " ++ cs (messageContent m)
        when (isPing m && not (fromBot m)) $ do
            void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
            void $ restCall (R.CreateMessage (messageChannelId m) "Pong!")
    Ready apiVer _ guilds _ _ _ pApp -> do
        liftIO $ putStrLn $ "Bot running with apiVersion=" ++ show apiVer
                ++ "\nInitial guilds: " ++ show (length guilds)
        registerCommands pApp
    InteractionCreate interaction -> handleInteraction interaction
    _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `isPrefixOf`) . toLower . messageContent
