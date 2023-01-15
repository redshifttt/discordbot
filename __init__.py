import os
import json
import aiohttp
import sqlite3
import discord
from discord.ext import commands

import hercules.commands
from hercules.commands import ask, serverinfo, avatar, userinfo, search, settings, help
import hercules.listeners
from hercules.listeners import server_logs
import hercules.helper.log as log

intents = discord.Intents.all()
bot = commands.Bot(command_prefix=".", intents=intents, help_command=None)

os.environ['TZ'] = 'UTC'

@bot.event
async def on_ready():
    log.in_log("INFO", "on_ready", f"Logged in as {bot.user}")

    await bot.add_cog(hercules.commands.ask.Ask(bot))
    await bot.add_cog(hercules.commands.serverinfo.ServerInfo(bot))
    await bot.add_cog(hercules.commands.avatar.Avatar(bot))
    await bot.add_cog(hercules.commands.userinfo.UserInfo(bot))
    await bot.add_cog(hercules.commands.search.Search(bot))
    await bot.add_cog(hercules.commands.settings.Settings(bot))
    await bot.add_cog(hercules.commands.help.Help(bot))

    await bot.add_cog(hercules.listeners.GenericListeners(bot))
    await bot.add_cog(hercules.listeners.server_logs.ServerEventLogging(bot))

    production_bot_id = 1001084456712544307

    db_con = sqlite3.connect("data.db")
    db_cur = db_con.cursor()

    res = db_cur.execute("SELECT name FROM sqlite_master WHERE name='servers'")

    if res.fetchone() == None:
        db_cur.execute("""
            CREATE TABLE servers(
                guild_id,
                traffic_channel,
                pins_channel,
                verification_channel,
                general_channel,
                logs_channel,
                join_message,
                leave_message,
                verification_message
            )
        """)
        db_con.commit()

    guilds = bot.guilds

    for guild in guilds:
        guild_id = guild.id
        res = db_cur.execute("SELECT guild_id FROM servers WHERE guild_id=?", (guild_id,))
        if res.fetchone() == None:
            print(f"Initialising {guild_id} in DB")
            db_cur.execute("INSERT INTO servers VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?)",
                            (guild_id, "null", "null", "null", "null", "null", "null", "null", "null",))
            db_con.commit()

        general_channel_in_server = discord.utils.find(lambda c: c.name == "general", guild.channels)

        if general_channel_in_server:
            res = db_cur.execute(f"UPDATE servers SET general_channel = {general_channel_in_server.id} WHERE guild_id={guild_id}")
            db_con.commit()

    db_con.close()

@bot.event
async def on_command_error(ctx, error):
    guild = ctx.guild
    guild_name = guild.name
    guild_id = guild.id
    channel = ctx.channel
    channel_name = channel.name
    channel_id = channel.id
    user = ctx.author
    user_id = user.id
    user_name = f"{user.name}#{user.discriminator}"

    log.in_log("ERROR", "on_command_error", f"guild_id={guild_id} guild_name='{guild_name}' channel_id={channel_id} channel_name='{channel_name}' user_id={user_id} user_tag={user_name} {error}")

    if ctx.message.content.startswith("._"):
        return

    embed_content = {
        "title": ":x: There was an error",
        "description": f"```{error}```"
    }

    embed = discord.Embed().from_dict(embed_content)

    await ctx.reply(embed=embed)

@bot.event
async def on_command_completion(ctx):
    guild = ctx.guild
    guild_name = guild.name
    guild_id = guild.id
    channel = ctx.channel
    channel_name = channel.name
    channel_id = channel.id
    user = ctx.author
    user_id = user.id
    user_name = f"{user.name}#{user.discriminator}"
    command_name = ctx.command.name
    command_args = [arg.message.content for arg in ctx.args[1:]]

    log.in_log("ERROR", "on_command", f"guild_id={guild_id} guild_name='{guild_name}' channel_id={channel_id} channel_name='{channel_name}' user_id={user_id} user_tag={user_name} command_name='{command_name}' command_args={command_args}")

with open("config.json", "r") as config:
    config = json.load(config)

real_bot = int(os.getenv("REAL"))

if not real_bot == 0:
    bot.run(config["dev_token"])

bot.run(config["token"])
