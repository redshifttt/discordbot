# Hercules Command/Systems Reference

[Add Hercules to your server!](https://discord.com/api/oauth2/authorize?client_id=997077128715706408&permissions=8&scope=bot)

[View the source code here](https://github.com/redshifttt/discordbot)

This page explains all of the commands for the bot and their usages.

## Systems

The cool part about the Hercules bot is the systems and the ability to modify parts of the systems to your liking.

### Join/Leave System

When turned on this system is modifiable with 3 [variables](#settings):

1. `traffic_channel` -- The channel join and leave messages are sent to.
2. `join_message` -- The message posted to that channel when a member joins the server.
3. `leave_message` -- The message posted to that channel when a member leaves the server.

If the `join_message` or `leave_message` are not set but the `traffic_channel` is then nothing will happen. Values must be set to work; goes for every system listed.

If the system is on and the `traffic_channel` variable is not set then the bot will attempt to send a System Message to the `general_channel` alerting that the system has been turned on but is not usable because this channel has not been set.

#### Keys

`join_message` and `leave_message` can both have a key called `{user}` in them which mentions the user within the respective message.

### Verification System

This system works in conjunction with the Join/Leave System but is **entirely optional**.

When turned on this system is modifiable with 2 variables:

1. `verification_channel` -- The verification channel in which a user will see when they first join a server.
2. `verification_message` -- The message that will be posted in the `verification_channel` mentioning the user when they join the server. This could include instructions on how to get in such as pinging an admin.

If there is no `verification_message` set then nothing will be posted to the `verification_channel`.

If the system is turned on but there is no value for the `verification_channel` then the bot will attempt to send a System Message to the `general_channel` highlighting that the system is on but nothing can be done till the correct variable is set.

#### Keys

Like `join_message` and `leave_message`, `verification_message` has a key called `{user}` in them which mentions the user within the respective message.

### Logs System

When turned on this system is modifiable with 1 variables:

1. `logs_channel` -- The channel the server logs are sent to.

The types of logs that the bot will send to the channel are as follows:

- Join and Leave messages; these are different from the Join/Leave System in that they provide more information.
- Server member updates which include:
    - Nickname changes
    - Roles being added or taken away
- Members being banned
- Deleted messages

In the future these may be toggleable so that they can individually be turned on and off.

If the system is turned on but the `logs_channel` is not set the bot will attempt to send a System Message to the `general_channel` alerting that the system has been turned on but the channel has not been setup ergo the system cannot function.

### Invite Nuker

When turned on this system will delete any server invite links that are posted to the server. This system has no configuration.

### Pins System

When turned on this system is modifiable with 2 variables:

1. `pins_webhook_url` -- The URL of the webhook used to post pinned messages to the pins channel. Note the `pins_channel` variable exists but is not used. More details below.
2. `pins_blacklist` -- A list of channel ID's where messages that are pinned will **not** be sent to the `pins_channel`.

#### Setting up the pins webhook

To set up the pins webhook, go to the channel you want all of the pinned messages to go to. Right click the channel -> Edit Channel -> Integrations -> Webhooks. Make a new webhook and copy the URL. Once this is done go back to the channel and [set](#settings) the `pins_webhook_url` variable

For the `pins_blacklist` variable see the last paragraph of the `./settings modify` command explanation.

For performance reasons only the **last 100 messages** in a channel can be pinned. The age of the messages does not matter; as long as it is within the 100 message threshold. This is due to Discord API message retrieval speeds which can take between 3-10 seconds.

## Commands

### Settings

`./settings`

: Running the command on its own will display the settings panel for your server. This has various **Systems** and **Settings** (variables) in it. `./settings` has **2** sub-commands: modify and remove.

`./settings modify <variable> <value>`

: This command will let you edit the contents of a `<variable>` in the settings panel. You can find what the `<variable>` is because the name of it will be inside a mini code block.

: For example in the **Settings** section at the bottom: "`general_channel`: 1089375254687786839"; this is what one of the settings could look like. The part with the underscores "`_`" is the `<variable>` in this case.

: For **Systems** you will see that they have on or off beside them. You can use the same commands with the values of `on` or `off` to be able to change them. Example: "**Join/Leave System** `join_leave_system`: Off" can be toggled like so: `./settings modify join_leave_system on`.

: **In the case of** `pins_blacklist` the values inside of it will be modified by just adding on whatever `<value>` is to whatever is already there. This can be completely removed with the next command.

`./settings remove <variable>`

: This command will take the `<variable>` and **completely remove whatever is inside of it**. This is useful if you just want to completely start over from something. `pins_blacklist` also requires using it as it is just a list which keeps getting added on to.

### Search

`./search <search term>`

: Do a web search for `<search term>`. The search API used is [Private Void Search](https://search.privatevoid.net) which is routed through an anonymous VPN (Mullvad) plus [no logs are kept](https://git.privatevoid.net/private-void/depot/-/blob/master/hosts/VEGAS/services/searxng/default.nix#L56). This will return the first **5** results found. It may take a second due to the anonymization factors used.

: **Example:**

: `./search hercules discord bot`

: **Aliases:**

: `./s`

### Info

#### Guild

Types of information displayed:

- Server name
- Member count with breakdown between users and bots
- Owner of the server
- Relative to today creation date
- Roles, channels and emote counts
- Server ID
- Server icon

`./info guild`

: Display server information for the server the command is ran in.

`./info guild index`

: List all of the servers that the bot is in by creation-date.

`./info guild query <id>`

: Get information on a guild that the bot is in by the guilds ID.

#### User

Types of information displayed:

- User tag (example: TestUser#1234)
- Relative today account creation-date
- Section with the user mention
- If the user has roles display the roles that they have
- The top role of the user
- The user's permissions within the guild. If the user has Administrator then only that will display because it overrules everything else.
- The user's profile picture
- The user's ID

`./info user`

: Gets information about yourself.

`./info user @usermention`
`./info user <id>`

: Gets information about a user via mention or ID.

### Avatar

`./avatar`

: Gets your own profile picture.

: **Aliases:**

: `./avi`, `./pfp`

`./avatar @usermention`

: Gets the profile picture of `@usermention`.

### Ask

`./ask <value> or <value>`
`./ask <value> or <value> or <value> or ...`

: Let the bot pick between values. This is random. Note "or" is needed between whatever values you use.

: **Example**

: `./ask 1 or 2`

: `./ask cat or dog or mouse or hamster`

: `./ask red car or blue car`
