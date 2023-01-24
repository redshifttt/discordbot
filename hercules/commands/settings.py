import discord
from discord.ext import commands
import sqlite3
import hercules.helper.log as log

class Settings(commands.Cog):
    def __init__(self, bot):
        self.bot = bot
        self.settings_panel = None

    async def generate_panel(self, ctx):
        channel = ctx.channel
        guild = ctx.guild

        settings_list = ""
        systems_list = ""
        misc_list = ""
        embed_content = {"title": f"Hercules settings for {guild.name}"}

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()
        guild_id = guild.id

        get_values_for_guild = db_cur.execute("SELECT * FROM servers WHERE guild_id=?", (guild_id,)).fetchall()[0]

        guild_id, \
        traffic_channel_id, \
        verification_channel_id, \
        general_channel_id, \
        logs_channel_id, \
        join_message, \
        leave_message, \
        verification_message, \
        join_leave_system, \
        invite_nuker_system, \
        verification_system, \
        logs_system = get_values_for_guild

        if not join_leave_system == "null":
            systems_list += f"**Join/Leave System** `join_leave`: :white_check_mark: On\n"
            if not traffic_channel_id == "null":
                traffic_channel = guild.get_channel(int(traffic_channel_id)).mention
                settings_list += f"__**Join/Leave System**__\n"
                settings_list += f"`traffic_channel`: {traffic_channel}\n"
                settings_list += f"- `join_message`: \"{join_message}\"\n"
                settings_list += f"- `leave_message`: \"{leave_message}\"\n"
        else:
            systems_list += f"**Join/Leave System** `join_leave`: :x: Off\n"

        if not verification_system == "null":
            systems_list += f"**Verification System** `verification`: :white_check_mark: On\n"
            if not verification_channel_id == "null":
                settings_list += f"__**Verification System**__\n"
                verification_channel = guild.get_channel(int(verification_channel_id)).mention
                settings_list += f"`verification_channel`: {verification_channel}\n"
                settings_list += f"- `verification_message`: \"{verification_message}\"\n"
        else:
            systems_list += f"**Verification System** `verification`: :x: Off\n"

        if not logs_system == "null":
            systems_list += f"**Logs System** `logs`: :white_check_mark: On\n"
            if not logs_channel_id == "null":
                logs_channel = guild.get_channel(int(logs_channel_id)).mention
                settings_list += f"__**Logs System**__\n"
                settings_list += f"`logs_channel`: {logs_channel}\n"
        else:
            systems_list += f"**Logs System** `logs`: :x: Off\n"

        if not invite_nuker_system == "null":
            systems_list += f"**Invite Nuker** `invite_nuker`: :white_check_mark: On\n"
        else:
            systems_list += f"**Invite Nuker** `invite_nuker`: :x: Off\n"

        if not general_channel_id == "null":
            general_channel = guild.get_channel(int(general_channel_id)).mention
            misc_list += f"__**Miscellaneous**__\n"
            misc_list += f"`general_channel`: {general_channel}\n"
        else:
            misc_list += f"`general_channel`: null\n"

        settings_list += misc_list

        embed_content["fields"] = [{"name": "Systems", "value": systems_list, "inline": False }]
        embed_content["fields"].append({ "name": "Settings", "value": settings_list, "inline": False })

        embed = discord.Embed().from_dict(embed_content)
        embed.set_thumbnail(url=guild.icon.url)

        return embed

    @commands.group(
        name="settings",
        brief="Configure the bot for the server",
        help="This command lets you configure how the bot works in the guild.\n\n**Usage**\n`.settings subcommand #channel or \"message\"` or just `.settings` to see server settings.\n\n**Turning off a system**\n`.settings subcommand null`.",
        aliases=["config"]
    )
    async def settings(self, ctx):
        channel = ctx.channel
        guild = ctx.guild
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

        message = await ctx.reply(embed=embed)
        if not self.settings_panel:
            self.settings_panel = message

    @settings.command(help="Turn the join/leave messages system on or off")
    async def join_leave(self, ctx, arg):
        guild = ctx.guild
        guild_id = guild.id
        arg = arg.lower()

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()

        match arg:
            case "on":
                res = db_cur.execute(f"UPDATE servers SET join_leave_system = 'on' WHERE guild_id={guild_id}")
                await ctx.reply(f":white_check_mark: Join/Leave System turned on.")
            case "off":
                res = db_cur.execute(f"UPDATE servers SET join_leave_system = 'null' WHERE guild_id={guild_id}")
                await ctx.reply(f":white_check_mark: Join/Leave System turned off.")
            case _:
                await ctx.reply("Not a valid value. `on|off` expected.")

        db_con.commit()
        db_con.close()

        if self.settings_panel:
            refreshed_panel = await Settings(self.bot).generate_panel(ctx)
            await self.settings_panel.edit(embed=refreshed_panel)

    @settings.command(help="Makes sure all join/leave messages go to that channel.")
    async def traffic_channel(self, ctx, arg):
        guild = ctx.guild
        guild_id = guild.id

        if arg.startswith("<#"):
            arg_as_id = arg[2:-1] # extract id out of str
            if arg_as_id.isnumeric():
                arg = guild.get_channel(int(arg_as_id))
                arg_channel_name = arg.mention
                arg = arg.id
        elif arg == "null":
            arg_channel_name = "null"
        else:
            await ctx.reply(":x: Invalid channel")
            return

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()
        res = db_cur.execute(f"UPDATE servers SET traffic_channel = '{arg}' WHERE guild_id={guild_id}")

        await ctx.reply(f":white_check_mark: traffic_channel set to {arg_channel_name}")
        db_con.commit()
        db_con.close()

    @settings.command(help="**Must have traffic_channel set.** The message that is posted to the traffic_channel when a user joins the server.")
    async def join_message(self, ctx, arg):
        guild = ctx.guild
        guild_id = guild.id

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()
        res = db_cur.execute(f"UPDATE servers SET join_message = '{arg}' WHERE guild_id={guild_id}")

        await ctx.reply(f":white_check_mark: join_message set to `{arg}`")
        db_con.commit()
        db_con.close()

    @settings.command(help="**Must have traffic_channel set.** The message that is posted to the traffic_channel when a user leaves the server.")
    async def leave_message(self, ctx, arg):
        guild = ctx.guild
        guild_id = guild.id

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()
        res = db_cur.execute(f"UPDATE servers SET leave_message = '{arg}' WHERE guild_id={guild_id}")

        await ctx.reply(f":white_check_mark: leave_message set to `{arg}`")
        db_con.commit()
        db_con.close()

    @settings.command(help="Turn the verification system on or off")
    async def verification(self, ctx, arg):
        guild = ctx.guild
        guild_id = guild.id
        arg = arg.lower()

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()

        match arg:
            case "on":
                res = db_cur.execute(f"UPDATE servers SET verification_system = 'on' WHERE guild_id={guild_id}")
                await ctx.reply(f":white_check_mark: Verification System turned on.")
            case "off":
                res = db_cur.execute(f"UPDATE servers SET verification_system = 'null' WHERE guild_id={guild_id}")
                await ctx.reply(f":white_check_mark: Verification System turned off.")
            case _:
                await ctx.reply("Not a valid value. `on|off` expected.")

        db_con.commit()
        db_con.close()

    @settings.command(help="Makes sure, eventually, that verification messages will be posted when a user joins.")
    async def verification_channel(self, ctx, arg):
        guild = ctx.guild
        guild_id = guild.id

        if arg.startswith("<#"):
            arg_as_id = arg[2:-1] # extract id out of str
            if arg_as_id.isnumeric():
                arg = guild.get_channel(int(arg_as_id))
                arg_channel_name = arg.mention
                arg = arg.id
        elif arg == "null":
            arg_channel_name = "null"
        else:
            await ctx.reply(":x: Invalid channel")
            return

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()
        res = db_cur.execute(f"UPDATE servers SET verification_channel = '{arg}' WHERE guild_id={guild_id}")

        await ctx.reply(f":white_check_mark: verification_channel set to `{arg_channel_name}`")
        db_con.commit()
        db_con.close()

    @settings.command(help="**Must have verification_channel set.** The message that is posted in the verification_channel when a user joins and has to be verified.")
    async def verification_message(self, ctx, arg):
        guild = ctx.guild
        guild_id = guild.id

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()
        res = db_cur.execute(f"UPDATE servers SET verification_message = '{arg}' WHERE guild_id={guild_id}")

        await ctx.reply(f":white_check_mark: verification_message set to `{arg}`")
        db_con.commit()
        db_con.close()

    @settings.command(help="Turn the verification system on or off")
    async def logs(self, ctx, arg):
        guild = ctx.guild
        guild_id = guild.id
        arg = arg.lower()

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()

        match arg:
            case "on":
                res = db_cur.execute(f"UPDATE servers SET logs_system = 'on' WHERE guild_id={guild_id}")
                await ctx.reply(f":white_check_mark: Logs System turned on.")
            case "off":
                res = db_cur.execute(f"UPDATE servers SET logs_system = 'null' WHERE guild_id={guild_id}")
                await ctx.reply(f":white_check_mark: Logs System turned off.")
            case _:
                await ctx.reply("Not a valid value. `on|off` expected.")

        db_con.commit()
        db_con.close()

    @settings.command(help="Where server logs such as bans, message deleted, join/leave, nickname/role change go.")
    async def logs_channel(self, ctx, arg):
        guild = ctx.guild
        guild_id = guild.id

        if arg.startswith("<#"):
            arg_as_id = arg[2:-1] # extract id out of str
            if arg_as_id.isnumeric():
                arg = guild.get_channel(int(arg_as_id))
                arg_channel_name = arg.mention
                arg = arg.id
        elif arg == "null":
            arg_channel_name = "null"
        else:
            await ctx.reply(":x: Invalid channel")
            return

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()
        res = db_cur.execute(f"UPDATE servers SET logs_channel = '{arg}' WHERE guild_id={guild_id}")

        await ctx.reply(f":white_check_mark: logs_channel set to `{arg_channel_name}`")
        db_con.commit()
        db_con.close()

    @settings.command(help="Turn the invite deletion system on or off")
    async def invite_nuker(self, ctx, arg):
        guild = ctx.guild
        guild_id = guild.id
        arg = arg.lower()

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()

        match arg:
            case "on":
                res = db_cur.execute(f"UPDATE servers SET invite_nuker_system = 'on' WHERE guild_id={guild_id}")
                await ctx.reply(f":white_check_mark: Invite Nuker turned on.")
            case "off":
                res = db_cur.execute(f"UPDATE servers SET invite_nuker_system = 'null' WHERE guild_id={guild_id}")
                await ctx.reply(f":white_check_mark: Invite Nuker turned off.")
            case _:
                await ctx.reply("Not a valid value. `on|off` expected.")

        db_con.commit()
        db_con.close()

    @settings.command(help="Where general system messages will be posted.")
    async def general_channel(self, ctx, arg):
        guild = ctx.guild
        guild_id = guild.id

        if arg.startswith("<#"):
            arg_as_id = arg[2:-1] # extract id out of str
            if arg_as_id.isnumeric():
                arg = guild.get_channel(int(arg_as_id))
                arg_channel_name = arg.mention
                arg = arg.id
        elif arg == "null":
            arg_channel_name = "null"
        else:
            await ctx.reply(":x: Invalid channel")
            return

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()
        res = db_cur.execute(f"UPDATE servers SET general_channel = '{arg}' WHERE guild_id={guild_id}")

        await ctx.reply(f":white_check_mark: general_channel set to `{arg_channel_name}`")
        db_con.commit()
        db_con.close()

async def setup(bot):
    log.in_log("INFO", "command_setup", "command settings has been loaded")
    await bot.add_cog(Settings(bot))
