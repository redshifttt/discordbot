# Hercules Discord Bot

The one bot to rule them all.

Hercules is an all-purpose bot with features that a lot of the usual bots have
and then some.

## Features

### Systems (Optional and Configurable)

- Messages that are pinned are sent to a channel to avoid the 50 pin message limit. *(channel configurable + optional)*
- Join/leave messages *(channel configurable + optional)*
- Verification system plus message on join *(channel configurable + optional)*
- Post a 4chan link and you get an embed of thread stats
- Message links also get a fancy embed which gives you stats on what the message is, who linked it, who's message it is, etc.

### Commands

- ask: ask the bot to randomly pick between 2 options
- serverinfo: display information about the server the bot is running in
- userinfo: display information about yourself or another user
- avatar: display the profile picture of yourself or another user
- search: search the web via [Private Void Search](https://search.privatevoid.net) (server-side the searches are routed through Mullvad VPN and [no logs](https://github.com/privatevoid-net/privatevoid-infrastructure/blob/bec93baa5aacdff4e09611da7c6ae859b40f4252/hosts/VEGAS/services/searxng/default.nix#L60) are kept)
- settings: manage your server-specific bot settings
