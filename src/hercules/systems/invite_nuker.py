import discord
from discord.ext import commands
import sqlite3
import hercules.helper.log as log
import hercules.helper.herculesdb as db
import datetime as dt

class InviteNuker(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.Cog.listener(name='on_message')
    async def invite_nuker(self, message):
        guild_id = message.guild.id

        db_connection, db_cursor = db.connect_to_db("data.db")

        row = db_cursor.execute("SELECT invite_nuker_system FROM servers WHERE guild_id = ?", (guild_id,)).fetchone()

        invite_nuker_system = row["invite_nuker_system"]

        db_connection.close()

        if invite_nuker_system == 1:
            if "discord.gg" in message.content or "discord.com/invite" in message.content:
                await message.delete()
                await message.author.timeout(dt.timedelta(minutes=1), reason=f"Advertising in #{message.channel.name}")

async def setup(bot):
    log.in_log("INFO", "listener_setup", "Invite Nuker system has been loaded")
    await bot.add_cog(InviteNuker(bot))
