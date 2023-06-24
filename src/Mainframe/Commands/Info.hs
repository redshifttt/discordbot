{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Mainframe.Commands.Info (cmdInfo) where

import           Universum
import           Mainframe.Types
import           Discord.Interactions
import           Discord
import qualified Discord.Requests as R
import Discord.Types
import Data.List (singleton)
import Data.Maybe
import Mainframe.Interactions
import qualified Mainframe.CommandParameters.Types as Param
import qualified Data.List as L

cmdInfo :: Command
cmdInfo = Command {
  commandName = "info",
  register = (fromJust $ createChatInput "info" "Show information about users or guilds") {
    createOptions = Just $ OptionsSubcommands [
      OptionSubcommandOrGroupSubcommand $ Param.subcommand {
        optionSubcommandName = "guild",
        optionSubcommandDescription = "Display info about the current guild"
      },
      OptionSubcommandOrGroupSubcommand $ Param.subcommand {
        optionSubcommandName = "user",
        optionSubcommandDescription = "Display info about the given user",
        optionSubcommandOptions = [ Param.user ]
      }
    ]
  },
  handler = handleInfo
}

handleInfo :: Interaction -> DiscordHandler ()
handleInfo int = do
  let opts = fromJust $ optionsData $ applicationCommandData int
  case opts of
    OptionsDataValues _ -> return ()
    OptionsDataSubcommands subcmds -> do
      case safeHead subcmds of
        Just (OptionDataSubcommandOrGroupSubcommand (OptionDataSubcommand "guild" _ _)) -> do
          case interactionGuildId int of
            Nothing -> respond_ int $ interactionResponseBasic "failed to get guild ID"
            Just gldId -> runInteraction int $ do
              putTextLn "before r"
              r <- restCall $ R.GetGuild gldId
              print r
              return $ r >>= \guild ->
                return ()
                --return $ do
                --  liftIO $ print "in guild"
                --  respond int $ interactionResponseBasic "test"
                -- return $ respond int $ InteractionResponseChannelMessage (interactionResponseMessageBasic "") {
                --   interactionResponseMessageEmbeds = let
                --     simpleCounter name valuesF = Just EmbedField {
                --       embedFieldName = name,
                --       embedFieldInline = Just False,
                --       embedFieldValue = show $ L.length $ valuesF guild
                --     }
                --   in Just $ singleton def {
                --     createEmbedAuthorName = "Guild Info",
                --     createEmbedTitle = guildName guild,
                --     createEmbedFields = catMaybes [
                --       guildMembers guild >>= \members -> Just EmbedField {
                --         embedFieldName = "Members",
                --         embedFieldInline = Just False,
                --         embedFieldValue = let
                --           memberCount = length members

                --           memberIsRegular member = fromMaybe False $ memberUser member >>= \user -> Just $ not $ userIsBot user

                --           memberCountRegular = length $ filter memberIsRegular members

                --         in show memberCount <> " (" <> show memberCountRegular <> " users, " <> show (memberCount - memberCountRegular) <> " bots)"
                --       },
                --       simpleCounter "Emojis" guildEmojis,
                --       simpleCounter "Roles" guildRoles,
                --       simpleCounter "Stickers" guildStickers
                --     ],
                --     createEmbedDescription = show $ guildFeatures guild,
                --     createEmbedFooterText = show gldId,
                --     createEmbedThumbnail = guildIcon guild >>= \icon -> Just $ CreateEmbedImageUrl $ "https://cdn.discordapp.com/icons/" <> show gldId <> "/" <> icon <> ".png"
                --   }
                -- }
        Just (OptionDataSubcommandOrGroupSubcommand (OptionDataSubcommand "user" _ _)) -> do
          case interactionGuildId int of
            Nothing -> respond_ int $ interactionResponseBasic "failed to get guild ID"
            Just gldId -> runInteraction int $ do
              r <- restCall $ R.GetGuild gldId
              return $ r >>= \guild ->
                return $ respond int $ InteractionResponseChannelMessage (interactionResponseMessageBasic "") {
                  interactionResponseMessageEmbeds = let
                    simpleCounter name valuesF = Just EmbedField {
                      embedFieldName = name,
                      embedFieldInline = Just False,
                      embedFieldValue = show $ L.length $ valuesF guild
                    }
                  in Just $ singleton def {
                    createEmbedAuthorName = "Guild Info",
                    createEmbedTitle = guildName guild,
                    createEmbedFields = catMaybes [
                      guildMembers guild >>= \members -> Just EmbedField {
                        embedFieldName = "Members",
                        embedFieldInline = Just False,
                        embedFieldValue = let
                          memberCount = length members

                          memberIsRegular member = fromMaybe False $ memberUser member >>= \user -> Just $ not $ userIsBot user

                          memberCountRegular = length $ filter memberIsRegular members

                        in show memberCount <> " (" <> show memberCountRegular <> " users, " <> show (memberCount - memberCountRegular) <> " bots)"
                      },
                      simpleCounter "Emojis" guildEmojis,
                      simpleCounter "Roles" guildRoles,
                      simpleCounter "Stickers" guildStickers
                    ],
                    createEmbedDescription = show $ guildFeatures guild,
                    createEmbedFooterText = show gldId,
                    createEmbedThumbnail = guildIcon guild >>= \icon -> Just $ CreateEmbedImageUrl $ "https://cdn.discordapp.com/icons/" <> show gldId <> "/" <> icon <> ".png"
                  }
                }
        _ -> print subcmds >> return ()
