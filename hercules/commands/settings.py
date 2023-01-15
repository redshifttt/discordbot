import discord
from discord.ext import commands
import sqlite3

class Settings(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.group(
        name="settings",
        brief="Configure the bot for the server",
        help="This command lets you configure how the bot works in the guild.\n\n**Usage**\n`.settings <subcommand> <value>` or just `.settings` to see server settings.\n\n**Turning off a system**\n`.settings <subcommand> null`.",
        aliases=["config"]
    )
    async def settings(self, ctx):
        channel = ctx.channel

        user_permissions = channel.permissions_for(ctx.author)
        if not user_permissions.manage_guild or not ctx.author.id == 356830440629207040:
            return

        if ctx.invoked_subcommand:
            return

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()
        guild_id = ctx.guild.id

        guild_id, traffic_channel, verification_channel, general_channel, logs_channel, join_message, leave_message, verification_message = db_cur.execute("SELECT * FROM servers WHERE guild_id=?", (guild_id,)).fetchall()[0]

        embed_content = {
            "title": f"Hercules settings for {ctx.guild.name}",
            "fields": [
                { "name": "┃ traffic_channel", "value": traffic_channel, "inline": False },
                { "name": "┃ verification_channel", "value": verification_channel, "inline": False },
                { "name": "┃ general_channel", "value": general_channel, "inline": False },
                { "name": "┃ logs_channel", "value": logs_channel, "inline": False },
            ]
        }

        # Visuals (just for looks purely)
        if not traffic_channel == "null":
            for i, v in enumerate(embed_content["fields"]):
                if v["name"] == "┃ traffic_channel":
                    embed_content["fields"].insert(i + 1, { "name": "→ join_message", "value": join_message, "inline": False })
                    embed_content["fields"].insert(i + 2, { "name": "→ leave_message", "value": leave_message, "inline": False })
        if not verification_channel == "null":
            for i, v in enumerate(embed_content["fields"]):
                if v["name"] == "┃ verification_channel":
                    embed_content["fields"].insert(i + 1, { "name": "→ verification_message", "value": verification_message, "inline": False })

        # Unfortunately the below doesn't work. Time to open myself up to SQL
        # injections - not like it matters though.
        # res = db_cur.execute("UPDATE servers SET ? = ? WHERE guild_id=?", (header, value, guild_id,))

        embed = discord.Embed().from_dict(embed_content)
        embed.set_thumbnail(url=ctx.guild.icon.url)

        await ctx.reply(embed=embed)

    @settings.command(help="Makes sure all join/leave messages go to that channel.")
    async def traffic_channel(self, ctx, arg):
        guild_id = ctx.guild.id
        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()
        res = db_cur.execute(f"UPDATE servers SET traffic_channel = '{arg}' WHERE guild_id={guild_id}")

        await ctx.reply(f":white_check_mark: traffic_channel set to `{arg}`")
        db_con.commit()
        db_con.close()

    @settings.command(help="Makes sure, eventually, that verification messages will be posted when a user joins.")
    async def verification_channel(self, ctx, arg):
        guild_id = ctx.guild.id
        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()
        res = db_cur.execute(f"UPDATE servers SET verification_channel = '{arg}' WHERE guild_id={guild_id}")

        await ctx.reply(f":white_check_mark: verification_channel set to `{arg}`")
        db_con.commit()
        db_con.close()

    @settings.command(help="Where general system messages will be posted.")
    async def general_channel(self, ctx, arg):
        guild_id = ctx.guild.id
        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()
        res = db_cur.execute(f"UPDATE servers SET general_channel = '{arg}' WHERE guild_id={guild_id}")

        await ctx.reply(f":white_check_mark: general_channel set to `{arg}`")
        db_con.commit()
        db_con.close()

    @settings.command(help="Where server logs such as bans, message deleted, join/leave, nickname/role change go.")
    async def logs_channel(self, ctx, arg):
        guild_id = ctx.guild.id
        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()
        res = db_cur.execute(f"UPDATE servers SET logs_channel = '{arg}' WHERE guild_id={guild_id}")

        await ctx.reply(f":white_check_mark: logs_channel set to `{arg}`")
        db_con.commit()
        db_con.close()

    @settings.command(help="**Must have traffic_channel set.** The message that is posted to the traffic_channel when a user joins the server.")
    async def join_message(self, ctx, arg):
        guild_id = ctx.guild.id
        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()
        res = db_cur.execute(f"UPDATE servers SET join_message = '{arg}' WHERE guild_id={guild_id}")

        await ctx.reply(f":white_check_mark: join_message set to `{arg}`")
        db_con.commit()
        db_con.close()


    @settings.command(help="**Must have traffic_channel set.** The message that is posted to the traffic_channel when a user leaves the server.")
    async def leave_message(self, ctx, arg):
        guild_id = ctx.guild.id
        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()
        res = db_cur.execute(f"UPDATE servers SET leave_message = '{arg}' WHERE guild_id={guild_id}")

        await ctx.reply(f":white_check_mark: leave_message set to `{arg}`")
        db_con.commit()
        db_con.close()


    @settings.command(help="**Must have verification_channel set.** The message that is posted in the verification_channel when a user joins and has to be verified.")
    async def verification_message(self, ctx, arg):
        guild_id = ctx.guild.id
        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()
        res = db_cur.execute(f"UPDATE servers SET verification_message = '{arg}' WHERE guild_id={guild_id}")

        await ctx.reply(f":white_check_mark: verification_message set to `{arg}`")
        db_con.commit()
        db_con.close()

