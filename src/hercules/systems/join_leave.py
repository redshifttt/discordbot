import discord
from discord.ext import commands
from tinydb import TinyDB, where

class JoinLeave(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    def key_check(self, member, str_check):
        split_message = str_check.split(" ")

        for n, k in enumerate(split_message):
            if k == "{user}":
                split_message[n] = member.mention

        return " ".join(split_message)

    @commands.Cog.listener()
    async def on_member_join(self, member):
        guild_id = member.guild.id

        db = TinyDB("db.json")
        guild_row = db.search(where("server_id") == guild_id)[0]

        join_leave_system = guild_row["join_leave_system"]
        traffic_channel = guild_row["traffic_channel"]
        join_message = guild_row["join_message"]
        verification_system = guild_row["verification_system"]
        verification_channel = guild_row["verification_channel"]
        verification_message = guild_row["verification_message"]

        if join_leave_system:
            if traffic_channel:
                traffic_channel = self.bot.get_channel(int(traffic_channel))

                if join_message:
                    join_message = JoinLeave(self.bot).key_check(member, join_message)
                    await traffic_channel.send(f"{join_message}")

        if verification_system:
            if verification_channel:
                verification_channel = self.bot.get_channel(int(verification_channel))

                if verification_message:
                    verification_message = JoinLeave(self.bot).key_check(member, verification_message)
                    await verification_channel.send(f"{verification_message}")

    @commands.Cog.listener()
    async def on_member_remove(self, member):
        guild_id = member.guild.id

        db = TinyDB("db.json")
        guild_row = db.search(where("server_id") == guild_id)[0]

        join_leave_system = guild_row["join_leave_system"]
        traffic_channel = guild_row["traffic_channel"]
        leave_message = guild_row["leave_message"]

        if join_leave_system:
            if traffic_channel:
                traffic_channel = self.bot.get_channel(int(traffic_channel))

                if leave_message:
                    leave_message = JoinLeave(self.bot).key_check(member, leave_message)
                    await traffic_channel.send(f"{leave_message}")

async def setup(bot):
    await bot.add_cog(JoinLeave(bot))
