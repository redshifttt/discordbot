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
        pins_blacklist = guild_row["pins_blacklist"]

        """
        JOIN AND LEAVE SYSTEM
        """
        join_leave_system = guild_row["join_leave_system"]
        match join_leave_system:
            case 1:
                systems += "**Join/Leave System** `join_leave_system`: :white_check_mark: On\n"
                settings += "__**Join/Leave System**__\n"
                settings += f"`traffic_channel`: {traffic_channel}\n"
                settings += f"- `join_message`: {join_message}\n"
                settings += f"- `leave_message`: {leave_message}\n"
            case _:
                systems += "**Join/Leave System** `join_leave_system`: :x: Off\n"

        """
        VERIFICATION SYSTEM
        """
        verification_system = guild_row["verification_system"]
        match verification_system:
            case 1:
                systems += "**Verification System** `verification_system`: :white_check_mark: On\n"
                settings += "__**Verification System**__\n"
                settings += f"`verification_channel`: {verification_channel}\n"
                settings += f"- `verification_message`: {verification_message}\n"
            case _:
                systems += "**Verification System** `verification_system`: :x: Off\n"

        """
        LOGS SYSTEM
        """
        logs_system = guild_row["logs_system"]
        match logs_system:
            case 1:
                systems += "**Logs System** `logs_system`: :white_check_mark: On\n"
                settings += "__**Logs System**__\n"
                settings += f"`logs_channel`: {logs_channel}\n"
            case _:
                systems += "**Logs System** `logs_system`: :x: Off\n"

        """
        INVITE NUKER SYSTEM
        """
        invite_nuker_system = guild_row["invite_nuker_system"]
        match invite_nuker_system:
            case 1:
                systems += "**Invite Nuker** `invite_nuker_system`: :white_check_mark: On\n"
            case _:
                systems += "**Invite Nuker** `invite_nuker_system`: :x: Off\n"

        """
        PINS SYSTEM
        """
        pins_system = guild_row["pins_system"]
        match pins_system:
            case 1:
                systems += "**Pins System** `pins_system`: :white_check_mark: On\n"
                settings += "__**Pins System**__\n"
                settings += f"`pins_channel`: {pins_channel}\n"
                settings += "`pins_webhook_url`: **[REDACTED]**\n"
                settings += f"`pins_blacklist`: {pins_blacklist}\n"
            case _:
                systems += "**Pins System** `pins_system`: :x: Off\n"
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

        embed_content = {
            "title": f":tools: Hercules settings for {guild.name}",
            "fields": [
                {"name": "Systems", "value": systems, "inline": False },
                {"name": "Settings", "value": settings, "inline": False }
            ]
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

        guild_row = db_cursor.execute("SELECT * FROM servers WHERE guild_id=?", (guild_id,)).fetchone()
        row_headers = guild_row.keys()

        if arg[0] not in row_headers:
            await ctx.reply(":x: Not a valid settings variable.")
            return

        if arg[0].endswith("_channel"):
            if not arg[1].isnumeric():
                await ctx.reply(":x: Value should be a channel ID")
                return

            sql_str = f"UPDATE servers SET {arg[0]} = ? WHERE guild_id = ?", (arg[1], guild_id,)
            await ctx.reply(f":white_check_mark: {arg[0]} set to {arg[1]}")
        elif arg[0].endswith("_system"):
            match arg[1]:
                case "on":
                    sql_str = f"UPDATE servers SET {arg[0]} = ? WHERE guild_id = ?", (True, guild_id,)
                    await ctx.reply(f":white_check_mark: {arg[0]} turned on.")
                case "off":
                    sql_str = f"UPDATE servers SET {arg[0]} = ? WHERE guild_id = ?", (False, guild_id,)
                    await ctx.reply(f":white_check_mark: {arg[0]} turned off.")
                case _:
                    await ctx.reply("Not a valid value. `on|off` expected.")
        elif arg[0].endswith("_blacklist"):
            has_value = db_cursor.execute(f"SELECT {arg[0]} FROM servers WHERE guild_id = ?", (guild_id,)).fetchone()[arg[0]]

            if has_value is not None:
                concat_arg = "," + str(arg[1])
                sql_str = f"UPDATE servers SET {arg[0]} = {arg[0]} || ? WHERE guild_id = ?;", (concat_arg, guild_id,)
            else:
                sql_str = f"UPDATE servers SET {arg[0]} = ? WHERE guild_id = ?", (arg[1], guild_id,)

            await ctx.reply(f":white_check_mark: {arg[0]} set to {arg[1]}")
        else:
            sql_str = f"UPDATE servers SET {arg[0]} = ? WHERE guild_id = ?", (arg[1], guild_id,)
            await ctx.reply(f":white_check_mark: {arg[0]} set to {arg[1]}")

        if sql_str:
            print(sql_str[0], sql_str[1])
            db_cursor.execute(sql_str[0], sql_str[1])
            db_connection.commit()

        db_connection.close()

    @settings.command()
    async def remove(self, ctx, arg):
        guild = ctx.guild
        guild_id = guild.id

        db_connection, db_cursor = db.connect_to_db("data.db")

        guild_row = db_cursor.execute("SELECT * FROM servers WHERE guild_id=?", (guild_id,)).fetchone()
        row_headers = guild_row.keys()

        if arg not in row_headers:
            await ctx.reply(":x: Not a valid settings variable.")
            return

        db_cursor.execute(f"UPDATE servers SET {arg} = ? WHERE guild_id = ?;", (None, guild_id,))
        await ctx.reply(f":white_check_mark: Removed the value of variable `{arg}`")

        db_connection.commit()

        db_connection.close()

async def setup(bot):
    log.in_log("INFO", "command_setup", "command settings has been loaded")
    await bot.add_cog(Settings(bot))
