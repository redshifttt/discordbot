import discord
from discord.ext import commands
import sqlite3
import hercules.helper.log as log
import hercules.helper.herculesdb as db

class Settings(commands.Cog):
    def __init__(self, bot):
        self.bot = bot
        self.settings_panel = None

    async def generate_panel(self, ctx):
        guild = ctx.guild

        settings = ""
        systems = ""
        misc = ""
        embed_content = {"title": f"Hercules settings for {guild.name}"}

        db_connection, db_cursor = db.connect_to_db("data.db")

        guild_id = guild.id
        guild_row = db_cursor.execute("SELECT * FROM servers WHERE guild_id=?", (guild_id,)).fetchone()

        traffic_channel = guild_row["traffic_channel"]
        join_message = guild_row["join_message"]
        leave_message = guild_row["leave_message"]

        verification_channel = guild_row["verification_channel"]
        verification_message = guild_row["verification_message"]

        general_channel = guild_row["general_channel"]

        logs_channel = guild_row["logs_channel"]

        pins_channel = guild_row["pins_channel"]

        """
        JOIN AND LEAVE SYSTEM
        """
        join_leave_system = guild_row["join_leave_system"]
        print(join_leave_system)
        match join_leave_system:
            case 1:
                systems += "**Join/Leave System** `join_leave`: :white_check_mark: On\n"
                settings += "__**Join/Leave System**__\n"
                settings += f"`traffic_channel`: {traffic_channel}\n"
                settings += f"- `join_message`: {join_message}\n"
                settings += f"- `leave_message`: {leave_message}\n"
            case _:
                systems += "**Join/Leave System** `join_leave`: :x: Off\n"

        """
        VERIFICATION SYSTEM
        """
        verification_system = guild_row["verification_system"]
        match verification_system:
            case 1:
                systems += "**Verification System** `verification`: :white_check_mark: On\n"
                settings += "__**Verification System**__\n"
                settings += f"`verification_channel`: {verification_channel}\n"
                settings += f"- `verification_message`: {verification_message}\n"
            case _:
                systems += "**Verification System** `verification`: :x: Off\n"

        """
        LOGS SYSTEM
        """
        logs_system = guild_row["logs_system"]
        match logs_system:
            case 1:
                systems += "**Logs System** `logs`: :white_check_mark: On\n"
                settings += "__**Logs System**__\n"
                settings += f"`logs_channel`: {logs_channel}\n"
            case _:
                systems += "**Logs System** `logs`: :x: Off\n"

        """
        INVITE NUKER SYSTEM
        """
        invite_nuker_system = guild_row["invite_nuker_system"]
        match invite_nuker_system:
            case 1:
                systems += "**Invite Nuker** `invite_nuker`: :white_check_mark: On\n"
            case _:
                systems += "**Invite Nuker** `invite_nuker`: :x: Off\n"

        """
        PINS SYSTEM
        """
        pins_system = guild_row["pins_system"]
        match pins_system:
            case 1:
                systems += "**Pins System** `pins`: :white_check_mark: On\n"
                settings += "__**Pins System**__\n"
                settings += f"`pins_channel`: {pins_channel}\n"
                settings += f"`pins_webhook_url`: **[REDACTED]**\n"
            case _:
                systems += "**Pins System** `pins`: :x: Off\n"
        """
        FINDING THE GENERAL CHANNEL
        """
        if not general_channel:
            misc += "`general_channel`: null\n"
        else:
            misc += "__**Miscellaneous**__\n"
            misc += f"`general_channel`: {general_channel}\n"

        # Combine the 2 kinds of settings
        settings += misc

        embed_content["fields"] = [{"name": "Systems", "value": systems, "inline": False }]
        embed_content["fields"].append({ "name": "Settings", "value": settings, "inline": False })

        embed = discord.Embed().from_dict(embed_content)
        embed.set_thumbnail(url=guild.icon.url)

        return embed

    @commands.group(
        name="settings",
        brief="Configure the bot for the server",
        help="This command lets you configure how the bot works in the guild.\n\n**Usage**\n`./settings subcommand key #channel or \"message\"` or just `./settings` to see server settings.\n\n**Turning off a system**\n`./settings remove key`.",
        aliases=["config"]
    )
    async def settings(self, ctx):
        channel = ctx.channel
        user_permissions = channel.permissions_for(ctx.author)
        if ctx.author.id == 356830440629207040:
            pass
        else:
            if not user_permissions.manage_guild:
                await ctx.reply(":x: You do not have the correct permissions needed to run this command. Needs `Manage Guild`.")
                return

        if ctx.invoked_subcommand:
            return

        embed = await Settings(self.bot).generate_panel(ctx)

        await ctx.reply(embed=embed)

    @settings.command()
    async def modify(self, ctx, *arg):
        if not arg:
            await ctx.reply(":x: No setting found.")
            return
        if not arg[1]:
            await ctx.reply(":x: No value for this setting found.")
            return

        db_connection, db_cursor = db.connect_to_db("data.db")

        guild = ctx.guild
        guild_id = guild.id

        match arg[0]:
            case "join_leave":
                match arg[1]:
                    case "on":
                        sql_str = "UPDATE servers SET join_leave_system = ? WHERE guild_id = ?", (True, guild_id,)
                        await ctx.reply(f":white_check_mark: Join/Leave System turned on.")
                    case "off":
                        sql_str = "UPDATE servers SET join_leave_system = ? WHERE guild_id = ?", (False, guild_id,)
                        await ctx.reply(f":white_check_mark: Join/Leave System turned off.")
                    case _:
                        await ctx.reply("Not a valid value. `on|off` expected.")
            case "logs":
                match arg[1]:
                    case "on":
                        sql_str = "UPDATE servers SET logs_system = ? WHERE guild_id = ?", (True, guild_id,)
                        await ctx.reply(f":white_check_mark: Logs System turned on.")
                    case "off":
                        sql_str = "UPDATE servers SET logs_system = ? WHERE guild_id = ?", (False, guild_id,)
                        await ctx.reply(f":white_check_mark: Logs System turned off.")
                    case _:
                        await ctx.reply("Not a valid value. `on|off` expected.")
            case "verification":
                match arg[1]:
                    case "on":
                        sql_str = "UPDATE servers SET verification_system = ? WHERE guild_id = ?", (True, guild_id,)
                        await ctx.reply(f":white_check_mark: Verification System turned on.")
                    case "off":
                        sql_str = "UPDATE servers SET verification_system = ? WHERE guild_id = ?", (False, guild_id,)
                        await ctx.reply(f":white_check_mark: Verification System turned off.")
                    case _:
                        await ctx.reply("Not a valid value. `on|off` expected.")
            case "invite_nuker":
                match arg[1]:
                    case "on":
                        sql_str = "UPDATE servers SET invite_nuker_system = ? WHERE guild_id = ?", (True, guild_id,)
                        await ctx.reply(f":white_check_mark: Invite Nuker System turned on.")
                    case "off":
                        sql_str = "UPDATE servers SET invite_nuker_system = ? WHERE guild_id = ?", (False, guild_id,)
                        await ctx.reply(f":white_check_mark: Invite Nuker System turned off.")
                    case _:
                        await ctx.reply("Not a valid value. `on|off` expected.")
            case "pins":
                match arg[1]:
                    case "on":
                        sql_str = "UPDATE servers SET pins_system = ? WHERE guild_id = ?", (True, guild_id,)
                        await ctx.reply(f":white_check_mark: Pins System turned on.")
                    case "off":
                        sql_str = "UPDATE servers SET pins_system = ? WHERE guild_id = ?", (False, guild_id,)
                        await ctx.reply(f":white_check_mark: Pins System turned off.")
                    case _:
                        await ctx.reply("Not a valid value. `on|off` expected.")
            case "traffic_channel":
                arg = arg[1]
                if arg.startswith("<#"):
                    arg_as_id = arg[2:-1]
                    if arg_as_id.isnumeric():
                        arg = guild.get_channel(int(arg_as_id))
                        arg_channel_name = arg.mention
                        arg = arg.id
                else:
                    await ctx.reply(":x: Invalid channel")
                    return

                sql_str = f"UPDATE servers SET traffic_channel = ? WHERE guild_id = ?", (arg, guild_id,)
                await ctx.reply(f":white_check_mark: traffic_channel set to {arg_channel_name}")
            case "verification_channel":
                arg = arg[1]
                if arg.startswith("<#"):
                    arg_as_id = arg[2:-1]
                    if arg_as_id.isnumeric():
                        arg = guild.get_channel(int(arg_as_id))
                        arg_channel_name = arg.mention
                        arg = arg.id
                else:
                    await ctx.reply(":x: Invalid channel")
                    return

                sql_str = f"UPDATE servers SET verification_channel = ? WHERE guild_id = ?", (arg, guild_id,)
                await ctx.reply(f":white_check_mark: verification_channel set to {arg_channel_name}")
            case "logs_channel":
                arg = arg[1]
                if arg.startswith("<#"):
                    arg_as_id = arg[2:-1]
                    if arg_as_id.isnumeric():
                        arg = guild.get_channel(int(arg_as_id))
                        arg_channel_name = arg.mention
                        arg = arg.id
                else:
                    await ctx.reply(":x: Invalid channel")
                    return

                sql_str = f"UPDATE servers SET logs_channel = ? WHERE guild_id = ?", (arg, guild_id,)
                await ctx.reply(f":white_check_mark: logs_channel set to {arg_channel_name}")
            case "general_channel":
                arg = arg[1]
                if arg.startswith("<#"):
                    arg_as_id = arg[2:-1]
                    if arg_as_id.isnumeric():
                        arg = guild.get_channel(int(arg_as_id))
                        arg_channel_name = arg.mention
                        arg = arg.id
                else:
                    await ctx.reply(":x: Invalid channel")
                    return

                sql_str = f"UPDATE servers SET general_channel = ? WHERE guild_id = ?", (arg, guild_id,)
                await ctx.reply(f":white_check_mark: general_channel set to {arg_channel_name}")
            case "join_message":
                message = arg[1]
                print(message)
                sql_str = f"UPDATE servers SET join_message = ? WHERE guild_id = ?", (message, guild_id,)
                await ctx.reply(f":white_check_mark: join_message set to `{message}`")
            case "leave_message":
                message = arg[1]
                sql_str = f"UPDATE servers SET leave_message = ? WHERE guild_id = ?", (message, guild_id,)
                await ctx.reply(f":white_check_mark: leave_message set to `{message}`")
            case "verification_message":
                message = arg[1]
                sql_str = f"UPDATE servers SET verification_message = ? WHERE guild_id = ?", (message, guild_id,)
                await ctx.reply(f":white_check_mark: verification_message set to `{message}`")
            case "pins_channel":
                arg = arg[1]
                sql_str = f"UPDATE servers SET pins_channel = ? WHERE guild_id = ?", (arg, guild_id,)
                await ctx.reply(f":white_check_mark: pins_channel set to `{arg}`")
            case "pins_webhook_url":
                arg = arg[1]
                sql_str = f"UPDATE servers SET pins_webhook_url = ? WHERE guild_id = ?", (arg, guild_id,)
                await ctx.reply(f":white_check_mark: pins_webhook_url set to `{arg}`")

        if sql_str:
            print(sql_str[0], sql_str[1])
            db_cursor.execute(sql_str[0], sql_str[1])
            db_connection.commit()

        db_connection.close()

async def setup(bot):
    log.in_log("INFO", "command_setup", "command settings has been loaded")
    await bot.add_cog(Settings(bot))
