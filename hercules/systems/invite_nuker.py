import discord
from discord.ext import commands
import sqlite3
import hercules.helper.log as log
import datetime as dt

class InviteNuker(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.Cog.listener(name='on_message')
    async def invite_nuker(self, message):
        guild_id = message.guild.id

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()

        general_channel = db_cur.execute("SELECT general_channel FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]
        invite_nuker_system = db_cur.execute("SELECT invite_nuker_system FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]

        db_con.close()

        if not invite_nuker_system == "null":
            if "discord.gg" in message.content or "discord.com/invite" in message.content:
                await message.delete()
                await message.author.timeout(dt.timedelta(minutes=1), reason=f"Advertising in #{message.channel.name}")

async def setup(bot):
    log.in_log("INFO", "listener_setup", "Invite Nuker system has been loaded")
    await bot.add_cog(InviteNuker(bot))
