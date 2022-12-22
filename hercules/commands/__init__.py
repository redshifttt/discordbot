import discord
from discord.ext import commands
import datetime as dt
import time
import requests
import humanize
import random
import json
import sqlite3

class User(commands.Cog):
    def __init__(self, bot):
        self.bot = bot
        self.time_format = "%a, %b %d %Y %H:%M"

    @commands.command(name="christmas")
    async def christmas(self, ctx):
        christmas = humanize.precisedelta(dt.datetime(2022, 12, 25, 0, 0, 0, 0) - dt.datetime.today(), minimum_unit="hours", format="%.0f")
        if dt.datetime.today().month == 12:
            reply = f"{christmas} till Christmas!"
        else:
            reply = "it's already christmas dummy!"

        await ctx.reply(reply)

