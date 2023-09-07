import os
import json
import time
import discord
from discord.ext import commands
from tinydb import TinyDB, Query, where
import sys
from prettytable import PrettyTable

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

    db = TinyDB("db.json")

    for guild in bot.guilds:
        guild_id = guild.id

        guild_id_in_db = db.search(where("server_id") == guild_id)
        if not guild_id_in_db:
            db.insert({
                "server_id": guild_id,
                "general_channel": None,
                "traffic_channel": None,
                "join_message": None,
                "leave_message": None,
                "verification_system": None,
                "verification_channel": None,
                "verification_message": None,
                "join_leave_system": None,
                "invite_nuker_system": None,
                "logs_system": None,
                "logs_channel": None,
                "pins_system": None,
                "pins_channel": None,
            })
            print(f"created db entry in servers for {guild.name}")

    print("bot ready")

    while True:
        line = sys.stdin.readline()
        if line.startswith(":c"):
            sys.stdout.write("Welcome to Hercules Command Mode!\n")
            while True:
                stdin = input(">>> ")
                match stdin:
                    case "guilds":
                        x = PrettyTable()
                        x.field_names = ["ID", "Guild name", "Member count", "Channel count", "Role count"]
                        for g in bot.guilds:
                            x.add_row([g.id, g.name, g.member_count, len(g.channels), len(g.roles)])
                        print(x)
                    case "q":
                        print("exiting command mode back to cli...")
                        break
                    case _:
                        pass

with open("config.json", "r", encoding="utf-8") as config:
    config = json.load(config)

real_bot = int(os.getenv("REAL"))

if not real_bot == 0:
    bot.run(config["dev_token"])

bot.run(config["token"])
