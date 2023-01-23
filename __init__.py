import os
import json
import aiohttp
import sqlite3
import discord
from discord.ext import commands
import hercules.helper.log as log

intents = discord.Intents.all()
bot = commands.Bot(command_prefix=".", intents=intents, help_command=None)

os.environ['TZ'] = 'UTC'

@bot.event
async def on_ready():
    log.in_log("INFO", "on_ready", f"Logged in as {bot.user}")

    for file in os.listdir("hercules/commands"):
        if "pycache" in file:
            continue
        await bot.load_extension("hercules.commands." + file[0:-3])

    for file in os.listdir("hercules/systems"):
        if "pycache" in file:
            continue
        await bot.load_extension("hercules.systems." + file[0:-3])

    production_bot_id = 1001084456712544307

    db_con = sqlite3.connect("data.db")
    db_cur = db_con.cursor()

    res = db_cur.execute("SELECT name FROM sqlite_master WHERE name='servers'")

    if res.fetchone() == None:
        db_cur.execute("""
            CREATE TABLE servers(
                guild_id,
                traffic_channel,
                verification_channel,
                general_channel,
                logs_channel,
                join_message,
                leave_message,
                verification_message,
                join_leave_system,
                invite_nuker_system,
                verification_system,
                logs_system
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
    command_arguments = ctx.message.content

    log.in_log("INFO", "on_command", f"guild_id={guild_id} guild_name='{guild_name}' channel_id={channel_id} channel_name='{channel_name}' user_id={user_id} user_tag={user_name} command_name='{command_name}' command_arguments='{command_arguments}'")

with open("config.json", "r") as config:
    config = json.load(config)

real_bot = int(os.getenv("REAL"))

if not real_bot == 0:
    bot.run(config["dev_token"])

bot.run(config["token"])
