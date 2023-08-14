import discord
from discord.ext import commands
from tinydb import TinyDB, where

class MiscListeners(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.Cog.listener(name='on_guild_join')
    async def db_server_init(self, guild):
        db = TinyDB("db.json")

        guild_id_in_db = db.search(where("server_id") == guild.id)
        if not guild_id_in_db:
            db.insert({
                "server_id": guild.id,
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

    @commands.Cog.listener(name='on_message')
    async def at_everyone(self, message):
        if message.mention_everyone:
            await message.reply("shut up")

async def setup(bot):
    await bot.add_cog(MiscListeners(bot))
