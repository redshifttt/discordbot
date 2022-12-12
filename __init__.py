import os
import json
import aiohttp
import sqlite3
import discord
from discord.ext import commands

import hercules.commands
import hercules.listeners

class BotConfig(object):
    def __init__(self, d):
        self.__dict__ = d

with open("config.json", "r") as config:
    config = json.load(config)
    conf = BotConfig(config)

intents = discord.Intents.all()
bot = commands.Bot(command_prefix=".", intents=intents)

os.environ['TZ'] = 'UTC'

@bot.event
async def on_ready():
    print(f"Logged in as {bot.user}")

    await bot.add_cog(hercules.commands.User(bot))
    await bot.add_cog(hercules.listeners.BotListeners(bot))

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
                logs_channel
            )
        """)
        db_con.commit()

    db_con.close()

@bot.event
async def on_command_error(ctx, error):
    embed_content = {
        "title": "There was an error",
        "colour": discord.Color.red(),
        "description": f"```{error}```"
    }

    embed = discord.Embed().from_dict(embed_content)

    await ctx.reply(embed=embed)

real_bot = int(os.getenv("REAL"))

if not real_bot == 0:
    bot.run(conf.dev_token)

bot.run(conf.token)
