import discord
from discord.ext import commands
import aiohttp
import datetime as dt
import sqlite3
import hercules.helper.herculesdb as db

class MiscListeners(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.Cog.listener(name='on_guild_join')
    async def db_server_init(self, guild):
        guild_id = guild.id

        db_connection, db_cursor = db.connect_to_db("data.db")

        db_cursor.execute("INSERT INTO servers VALUES(?)", (guild_id,))
        db_connection.commit()

        server_count = len(self.bot.guilds)
        user_count = len(self.bot.users)
        watching = discord.Activity(type=discord.ActivityType.watching, name=f"{server_count} servers and {user_count} users.")
        await self.bot.change_presence(activity=watching)

    @commands.Cog.listener(name='on_message')
    async def at_everyone(self, message):
        if message.mention_everyone:
            await message.reply("shut up")

async def setup(bot):
    await bot.add_cog(MiscListeners(bot))
