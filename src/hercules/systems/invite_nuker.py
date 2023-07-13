import discord
from discord.ext import commands
import sqlite3
import hercules.helper.herculesdb as db
import datetime as dt

class InviteNuker(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.Cog.listener(name='on_message')
    async def invite_nuker(self, message):
        db_connection, db_cursor = db.connect_to_db("data.db")
        row = db_cursor.execute("SELECT * FROM servers WHERE guild_id = ?", (message.guild.id,)).fetchone()
        invite_nuker_system = row["invite_nuker_system"]
        db_connection.close()

        triggers = ["discord.gg", "discord.com/invite"]

        if invite_nuker_system == 1:
            for t in triggers:
                if t in message.content and not message.author.guild_permissions.administrator:
                    await message.delete()
                    await message.author.timeout(dt.timedelta(minutes=1), reason=f"Advertising in #{message.channel.name}")

async def setup(bot):
    await bot.add_cog(InviteNuker(bot))
