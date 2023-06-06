import os
import json
import sqlite3
import time
import discord
from discord.ext import commands
import hercules.helper.log as log
import hercules.helper.herculesdb as db
import inspect

intents = discord.Intents.all()
bot = commands.Bot(
    command_prefix="./",
    intents=intents,
    help_command=None,
    allowed_mentions=discord.AllowedMentions(everyone=False, roles=False)
)

os.environ['TZ'] = 'UTC'

@bot.event
async def on_ready():
    bot_start_load_time = time.time()
    log.in_log("INFO", "on_ready", f"Logged in as {bot.user}")

    for file in os.listdir("hercules/commands"):
        if "pycache" in file:
            continue
        await bot.load_extension("hercules.commands." + file[0:-3])

    for file in os.listdir("hercules/systems"):
        if "pycache" in file:
            continue
        await bot.load_extension("hercules.systems." + file[0:-3])

    db_connection, db_cursor = db.connect_to_db("data.db")

    has_servers_db = db_cursor.execute("SELECT name FROM sqlite_master WHERE name='servers'").fetchone()

    if has_servers_db is None:
        db_cursor.execute("""
        CREATE TABLE
        servers(
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

    guilds_bot_is_in = bot.guilds

    for guild in guilds_bot_is_in:
        guild_id = guild.id
        guild_name = guild.name

        guild_id_in_db = db_cursor.execute("SELECT guild_id FROM servers WHERE guild_id=?", (guild_id,)).fetchone()

        if guild_id_in_db is None:
            log.in_log("INFO", "guild_db_init", f"initializing {guild_name} ({guild_id}) in DB...")
            db_cursor.execute("INSERT INTO servers (guild_id) VALUES (?)", (guild_id,))
            db_connection.commit()

        general_channel_in_server = discord.utils.find(lambda c: c.name == "general", guild.channels)

        if general_channel_in_server:
            db_cursor.execute(f"UPDATE servers SET general_channel = ? WHERE guild_id=?", (general_channel_in_server.id, guild_id))
            db_connection.commit()

    has_pins_db = db_cursor.execute("SELECT name FROM sqlite_master WHERE name='pins'").fetchone()

    if has_pins_db is None:
        db_cursor.execute("""CREATE TABLE pins( guild_id, pinned_message_id, pinned_user_id, pin_content, pin_attachments)""")
        log.in_log("INFO", "pins_db_init", "creating the pins table in DB...")
        db_connection.commit()

    db_connection.close()

    bot_finish_load_time = time.time()
    log.in_log("INFO", "benchmark", f"Finished bot startup in {bot_finish_load_time - bot_start_load_time:.3f} seconds")

with open("config.json", "r", encoding="utf-8") as config:
    config = json.load(config)

real_bot = int(os.getenv("REAL"))

if not real_bot == 0:
    bot.run(config["dev_token"])

bot.run(config["token"])
