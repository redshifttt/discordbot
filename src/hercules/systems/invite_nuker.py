import discord
from discord.ext import commands
import datetime as dt
from tinydb import TinyDB, where

class InviteNuker(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.Cog.listener(name='on_message')
    async def invite_nuker(self, message):
        db = TinyDB("db.json")
        row = db.search(where("server_id") == message.guild.id)[0]

        invite_nuker_system = row["invite_nuker_system"]

        if invite_nuker_system:
            triggers = ["discord.gg", "discord.com/invite"]

            for t in triggers:
                if t in message.content and not message.author.guild_permissions.administrator:
                    await message.delete()
                    await message.author.timeout(dt.timedelta(minutes=1), reason=f"Advertising in #{message.channel.name}")

    @commands.Cog.listener(name='on_message_edit')
    async def edit_invite_nuker(self, before, after):
        message = after

        db = TinyDB("db.json")
        row = db.search(where("server_id") == message.guild.id)[0]

        invite_nuker_system = row["invite_nuker_system"]

        if invite_nuker_system:
            triggers = ["discord.gg", "discord.com/invite"]

            for t in triggers:
                if t in message.content and not message.author.guild_permissions.administrator:
                    await message.delete()
                    await message.author.timeout(dt.timedelta(minutes=1), reason=f"Advertising in #{message.channel.name}")

async def setup(bot):
    await bot.add_cog(InviteNuker(bot))
