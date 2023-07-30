import discord
from discord.ext import commands
import aiohttp
import datetime as dt
import sqlite3
import hercules.helper.herculesdb as db
import random

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

    # GwaGwa-specific
    @commands.Cog.listener(name='on_member_update')
    async def bro_role_adder(self, before, after):
        if not before.guild.id == 1049498534476517447:
            return

        bro_role_id = 1052793333119991849
        new_roles_by_id = [r.id for r in after.roles]
        old_roles_by_id = [r.id for r in before.roles]
        had_bro_role_added = False

        if bro_role_id in new_roles_by_id and not bro_role_id in old_roles_by_id:
            had_bro_role_added = True
        else:
            return

        if len(after.roles) > len(before.roles) and had_bro_role_added:
            new_member = after
            welcome_channel = self.bot.get_channel(1049498535365705761)
            question = random.choice(welcome_questions)
            question_split = question.split(" ")

            for n, k in enumerate(question_split):
                if k.startswith("{user}"):
                    question_split[n] = new_member.mention

            print(question_split)
            question = " ".join(question_split)
            await welcome_channel.send(question)

        had_bro_role_added = False

welcome_questions = [
    "What's good with it, {user}?",
    "Welcome, {user}, to the ONLY Discord server.",
    "You have been chosen, {user}. Be grateful.",
    "First act as the newest GwaGwaCel, {user}. Post balls.",
    "You will do well as a servant of GwaGwa, {user}.",
    "Hey, {user}, tell us what brought you here!",
    "So, what's your story, {user}?"
]

async def setup(bot):
    await bot.add_cog(MiscListeners(bot))
