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

