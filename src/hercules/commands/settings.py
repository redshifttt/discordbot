import discord
from discord.ext import commands
import sqlite3
import hercules.helper.herculesdb as db

class Settings(commands.Cog):
    def __init__(self, bot):
        self.bot = bot
        self.settings_panel = None

    async def generate_panel(self, ctx):
        guild = ctx.guild

        settings_str = ""
        systems_str = ""
        misc = ""

        db_connection, db_cursor = db.connect_to_db("data.db")

        guild_id = guild.id
        guild_settings = db_cursor.execute("SELECT * FROM servers WHERE guild_id=?", (guild_id,)).fetchone()

        # Systems
        join_leave_system = guild_settings["join_leave_system"]
        verification_system = guild_settings["verification_system"]
        logs_system = guild_settings["logs_system"]
        invite_nuker_system = guild_settings["invite_nuker_system"]
        pins_system = guild_settings["pins_system"]

        # Settings
        traffic_channel = guild_settings["traffic_channel"]
        join_message = guild_settings["join_message"]
        leave_message = guild_settings["leave_message"]
        verification_channel = guild_settings["verification_channel"]
        verification_message = guild_settings["verification_message"]
        general_channel = guild_settings["general_channel"]
        logs_channel = guild_settings["logs_channel"]
        pins_channel = guild_settings["pins_channel"]
        pins_blacklist = guild_settings["pins_blacklist"]

        match join_leave_system:
            case 1:
                systems_str += "**`join_leave_system`**: :white_check_mark: On\n"

                settings_str += "**Join/Leave System**\n"
                settings_str += f"**`traffic_channel`**: {traffic_channel}\n"
                settings_str += f"**`join_message`**: {join_message}\n"
                settings_str += f"**`leave_message`**: {leave_message}\n"
                settings_str += "\n"
            case _:
                systems_str += "**`join_leave_system`**: :no_entry: Off\n"

        match verification_system:
            case 1:
                systems_str += "**`verification_system`**: :white_check_mark: On\n"

                settings_str += "**Verification System**\n"
                settings_str += f"**`verification_channel`**: {verification_channel}\n"
                settings_str += f"**`verification_message`**: {verification_message}\n"
                settings_str += "\n"
            case _:
                systems_str += "**`verification_system`**: :no_entry: Off\n"

        match logs_system:
            case 1:
                systems_str += "**`logs_system`**: :white_check_mark: On\n"

                settings_str += "**Logs System**\n"
                settings_str += f"**`logs_channel`**: {logs_channel}\n"
                settings_str += "\n"
            case _:
                systems_str += "**`logs_system`**: :no_entry: Off\n"

        match invite_nuker_system:
            case 1:
                systems_str += "**`invite_nuker_system`**: :white_check_mark: On\n"
            case _:
                systems_str += "**`invite_nuker_system`**: :no_entry: Off\n"

        match pins_system:
            case 1:
                systems_str += "**`pins_system`**: :white_check_mark: On\n"

                settings_str += "**Pins System**\n"
                settings_str += f"**`pins_channel`**: {pins_channel}\n"
                settings_str += "**`pins_webhook_url`**: **[REDACTED]**\n"
                settings_str += f"**`pins_blacklist`**: {pins_blacklist}\n"
                settings_str += "\n"
            case _:
                systems_str += "**`pins_system`**: :no_entry: Off\n"

        if not general_channel:
            misc += "**`general_channel`**: None\n"
        else:
            misc += "**Miscellaneous**\n"
            misc += f"**`general_channel`**: {general_channel}\n"

        settings_str += misc

        embed_content = {
            "title": f":tools: Hercules settings for {guild.name}",
            "fields": [
                {"name": "Systems", "value": systems_str, "inline": False },
                {"name": "Settings", "value": settings_str, "inline": False }
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
                await ctx.reply(":no_entry: You do not have the correct permissions needed to run this command. Needs `Manage Guild`.")
                return

        if ctx.invoked_subcommand:
            return

        embed = await Settings(self.bot).generate_panel(ctx)

        await ctx.reply(embed=embed)

    @settings.command()
    async def modify(self, ctx, *arg):
        if not arg:
            await ctx.reply(":no_entry: No setting found.")
            return
        if not arg[1]:
            await ctx.reply(":no_entry: No value for this setting found.")
            return

        settings_key = arg[0]
        settings_value = arg[1]

        db_connection, db_cursor = db.connect_to_db("data.db")

        guild = ctx.guild
        guild_id = guild.id

        guild_row = db_cursor.execute("SELECT * FROM servers WHERE guild_id=?", (guild_id,)).fetchone()
        row_headers = guild_row.keys()

        if settings_key not in row_headers:
            await ctx.reply(":no_entry: Not a valid settings variable.")
            return

        # Channel keys
        if settings_key.endswith("_channel"):
            if not settings_value.isnumeric():
                await ctx.reply(":no_entry: Value should be a channel ID")
                return

            sql_tuple = f"UPDATE servers SET {settings_key} = ? WHERE guild_id = ?", (settings_value, guild_id,)
            await ctx.reply(f":white_check_mark: {settings_key} set to {settings_value}")

        # System keys
        if settings_key.endswith("_system"):
            match settings_value:
                case "on":
                    sql_tuple = f"UPDATE servers SET {settings_key} = ? WHERE guild_id = ?", (True, guild_id,)
                    await ctx.reply(f":white_check_mark: {settings_key} turned on.")
                case "off":
                    sql_tuple = f"UPDATE servers SET {settings_key} = ? WHERE guild_id = ?", (False, guild_id,)
                    await ctx.reply(f":white_check_mark: {settings_key} turned off.")
                case _:
                    await ctx.reply("Not a valid value. `on|off` expected.")

        # Blacklist keys
        if settings_key.endswith("_blacklist"):
            guild_has_value = db_cursor.execute(f"SELECT {settings_key} FROM servers WHERE guild_id = ?", (guild_id,)).fetchone()[settings_key]

            if guild_has_value is not None:
                concat_arg = "," + str(settings_value)
                sql_tuple = f"UPDATE servers SET {settings_key} = {settings_key} || ? WHERE guild_id = ?;", (concat_arg, guild_id,)
            else:
                sql_tuple = f"UPDATE servers SET {settings_key} = ? WHERE guild_id = ?", (settings_value, guild_id,)

            await ctx.reply(f":white_check_mark: {settings_key} set to {settings_value}")

        if not settings_key.endswith("_blacklist") or not settings_key.endswith("_system") or not settings_key.endswith("_channel"):
            sql_tuple = f"UPDATE servers SET {settings_key} = ? WHERE guild_id = ?", (settings_value, guild_id,)
            await ctx.reply(f":white_check_mark: {settings_key} set to {settings_value}")

        if sql_tuple:
            print(sql_tuple[0], sql_tuple[1])
            db_cursor.execute(sql_tuple[0], sql_tuple[1])
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
            await ctx.reply(":no_entry: Not a valid settings variable.")
            return

        db_cursor.execute(f"UPDATE servers SET {arg} = ? WHERE guild_id = ?;", (None, guild_id,))
        await ctx.reply(f":white_check_mark: Removed the value of variable `{arg}`")

        db_connection.commit()
        db_connection.close()

async def setup(bot):
    await bot.add_cog(Settings(bot))
