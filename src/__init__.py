import os
import json
import sqlite3
import time
import discord
from discord.ext import commands
import hercules.helper.herculesdb as db
import inspect

os.environ['TZ'] = 'UTC'

intents = discord.Intents.all()
bot = commands.Bot(command_prefix="./", intents=intents, help_command=None, allowed_mentions=discord.AllowedMentions(everyone=False, roles=False))

@bot.event
async def on_ready():
    print(f"logged in as {str(bot.user)}")
    for file in os.listdir("hercules/commands"):
        if "pycache" in file:
            continue
        await bot.load_extension("hercules.commands." + file[0:-3])
        print(f"loaded commands {file[0:-3]}")

    for file in os.listdir("hercules/systems"):
        if "pycache" in file:
            continue
        await bot.load_extension("hercules.systems." + file[0:-3])
        print(f"loaded systems {file[0:-3]}")

    db_connection, db_cursor = db.connect_to_db("data.db")

    print("checking for servers db")
    has_servers_db = db_cursor.execute("SELECT name FROM sqlite_master WHERE name='servers'").fetchone()
    if has_servers_db is None:
        db_cursor.execute("""CREATE TABLE servers(
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
            logs_system,
            pins_system
            pins_channel,
            pins_blacklist
        )
        """)
        db_connection.commit()
        print("created servers db")

    has_pins_db = db_cursor.execute("SELECT name FROM sqlite_master WHERE name='pins'").fetchone()
    if has_pins_db is None:
        db_cursor.execute("CREATE TABLE pins(guild_id, pinned_message_id, pinned_user_id, pin_content, pin_attachments)")
        db_connection.commit()
        print("created pins db")

    guilds_bot_is_in = bot.guilds
    for guild in guilds_bot_is_in:
        guild_id = guild.id

        guild_id_in_db = db_cursor.execute("SELECT guild_id FROM servers WHERE guild_id=?", (guild_id,)).fetchone()
        if guild_id_in_db is None:
            db_cursor.execute("INSERT INTO servers (guild_id) VALUES (?)", (guild_id,))
            db_connection.commit()
            print(f"created db entry in servers for {guild.name}")

    db_connection.close()

    print("bot ready")

with open("config.json", "r", encoding="utf-8") as config:
    config = json.load(config)

real_bot = int(os.getenv("REAL"))

if not real_bot == 0:
    bot.run(config["dev_token"])

bot.run(config["token"])
