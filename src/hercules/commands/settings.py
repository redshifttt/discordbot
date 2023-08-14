import discord
from discord.ext import commands
from tinydb import TinyDB, where

class Settings(commands.Cog):
    def __init__(self, bot):
        self.bot = bot
        self.settings_panel = None

    async def generate_panel(self, ctx):
        guild = ctx.guild

        cfg_on = ""
        cfg_off = ""

        db = TinyDB("db.json")

        guild_id = guild.id
        guild_settings = db.search(where("server_id") == guild.id)[0]

        match guild_settings['join_leave_system']:
            case True:
                cfg_on += "**`join_leave_system`**: :white_check_mark: On\n"
                cfg_on += f"- **`traffic_channel`**: {guild_settings['traffic_channel']}\n"
                cfg_on += f"- **`join_message`**: {guild_settings['join_message']}\n"
                cfg_on += f"- **`leave_message`**: {guild_settings['leave_message']}\n"
                cfg_on += "\n"
            case _:
                cfg_off += "**`join_leave_system`**: :no_entry: Off\n"
                cfg_off += "\n"

        match guild_settings['verification_system']:
            case True:
                cfg_on += "**`verification_system`**: :white_check_mark: On\n"
                cfg_on += f"- **`verification_channel`**: {guild_settings['verification_channel']}\n"
                cfg_on += f"- **`verification_message`**: {guild_settings['verification_message']}\n"
                cfg_on += "\n"
            case _:
                cfg_off += "**`verification_system`**: :no_entry: Off\n"
                cfg_off += "\n"

        match guild_settings['logs_system']:
            case True:
                cfg_on += "**`logs_system`**: :white_check_mark: On\n"
                cfg_on += f"- **`logs_channel`**: {guild_settings['logs_channel']}\n"
                cfg_on += "\n"
            case _:
                cfg_off += "**`logs_system`**: :no_entry: Off\n"
                cfg_off += "\n"

        match guild_settings['invite_nuker_system']:
            case True:
                cfg_on += "**`invite_nuker_system`**: :white_check_mark: On\n"
            case _:
                cfg_off += "**`invite_nuker_system`**: :no_entry: Off\n"
                cfg_off += "\n"

        match guild_settings['pins_system']:
            case True:
                cfg_on += "**`pins_system`**: :white_check_mark: On\n"
                cfg_on += f"- **`pins_channel`**: {guild_settings['pins_channel']}\n"
                cfg_on += "- **`pins_webhook_url`**: **[REDACTED]**\n"
                cfg_on += "\n"
            case _:
                cfg_off += "**`pins_system`**: :no_entry: Off\n"
                cfg_off += "\n"

        cfg = cfg_on + cfg_off

        embed_content = {
            "title": f":tools: Hercules settings for {guild.name}",
            "fields": [{"name": "Settings", "value": cfg, "inline": True }]
        }

        embed = discord.Embed().from_dict(embed_content)

        guild_icon = guild.icon
        if guild_icon:
            embed.set_thumbnail(url=guild_icon.url)

        return embed

    @commands.group(aliases=["config"])
    async def settings(self, ctx):
        channel = ctx.channel
        user_permissions = channel.permissions_for(ctx.author)
        if ctx.author.id == 356830440629207040:
            pass
        else:
            if not user_permissions.manage_guild:
                await ctx.reply(":no_entry: You do not have the correct permissions needed to run this command. Needs `Manage Guild`.")
                return

        if ctx.invoked_subcommand:
            return

        embed = await Settings(self.bot).generate_panel(ctx)

        await ctx.reply(embed=embed)

    @settings.command()
    async def modify(self, ctx, *arg):
        if len(arg) < 2:
            await ctx.reply(":no_entry: Not enough arguments. Refer to `./help`")
            return

        settings_key = arg[0]
        settings_value = arg[1]

        guild = ctx.guild
        guild_id = guild.id

        db = TinyDB("db.json")
        guild_row = db.search(where("server_id") == guild.id)[0]

        if settings_key not in guild_row.keys():
            await ctx.reply(":no_entry: Not a valid settings variable.")
            return

        # System keys
        if settings_key.endswith("_system"):
            match settings_value:
                case "on":
                    db.update({settings_key: True}, where("server_id") == guild.id)
                    await ctx.reply(f":white_check_mark: **`{settings_key}`** turned on.")
                case "off":
                    db.update({settings_key: False}, where("server_id") == guild.id)
                    await ctx.reply(f":white_check_mark: **`{settings_key}`** turned off.")
            return

        # Channel keys
        if settings_key.endswith("_channel"):
            # Only need to check if it's numeric, otherwise move on to default behaviour
            if not settings_value.isnumeric():
                await ctx.reply(":no_entry: Value should be a channel ID")
                return

        # Default behaviour
        db.update({settings_key: settings_value}, where("server_id") == guild.id)
        await ctx.reply(f":white_check_mark: **`{settings_key}`** set to {settings_value}")

    @settings.command()
    async def remove(self, ctx, arg):
        guild = ctx.guild
        guild_id = guild.id

        db = TinyDB("db.json")
        guild_row = db.search(where("server_id") == guild.id)[0]

        if arg not in guild_row.keys():
            await ctx.reply(":no_entry: Not a valid settings variable.")
            return

        db.update({arg: None}, where("server_id") == guild.id)
        await ctx.reply(f":white_check_mark: Removed the value of variable **`{arg}`**")

async def setup(bot):
    await bot.add_cog(Settings(bot))
