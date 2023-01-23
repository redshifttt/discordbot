import discord
from discord.ext import commands
import sqlite3
import hercules.helper.log as log

class InviteNuker(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.Cog.listener(name='on_message')
    async def invite_nuker(self, message):
        if "discord.gg" in message.content:
            await message.delete()
            await message.author.timeout(dt.timedelta(minutes=1), reason=f"Advertising in #{message.channel.name}")

async def setup(bot):
    log.in_log("INFO", "listener_setup", "Invite Nuker system has been loaded")
    await bot.add_cog(InviteNuker(bot))
