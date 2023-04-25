import discord
from discord.ext import commands
import sqlite3
import hercules.helper.log as log
import hercules.helper.herculesdb as db

class JoinLeave(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    def key_check(self, member, str_check):
        split_message = str_check.split(" ")

        for n, k in enumerate(split_message):
            match k:
                case "{user}":
                    split_message[n] = member.mention

        return " ".join(split_message)

    @commands.Cog.listener()
    async def on_member_join(self, member):
        guild_id = member.guild.id

        db_connection, db_cursor = db.connect_to_db("data.db")

        guild_row = db_cursor.execute("SELECT general_channel, join_leave_system, traffic_channel, join_message, verification_system, verification_channel, verification_message FROM servers WHERE guild_id = ?", (guild_id,)).fetchone()

        general_channel = guild_row["general_channel"]
        join_leave_system = guild_row["join_leave_system"]
        traffic_channel = guild_row["traffic_channel"]
        join_message = guild_row["join_message"]
        verification_system = guild_row["verification_system"]
        verification_channel = guild_row["verification_channel"]
        verification_message = guild_row["verification_message"]

        if general_channel is not None:
            general_channel = self.bot.get_channel(int(general_channel))

        if join_leave_system == 1:
            if traffic_channel is not None:
                traffic_channel = self.bot.get_channel(int(traffic_channel))

                if join_message is not None:
                    join_message = JoinLeave(self.bot).key_check(member, join_message)
                    await traffic_channel.send(f"{join_message}")
            else:
                await general_channel.send(":warning: **System Message**: The Join/Leave system has been turned on but there is no `traffic_channel` set.")

        if verification_system == 1:
            if verification_channel is not None:
                verification_channel = self.bot.get_channel(int(verification_channel))

                if verification_message is not None:
                    verification_message = JoinLeave(self.bot).key_check(member, verification_message)
                    await verification_channel.send(f"{verification_message}")
            else:
                await general_channel.send(":warning: **System Message**: The Verification System has been turned on but there is no `verification_channel` set.")

        db_connection.close()

    @commands.Cog.listener()
    async def on_message(self, message):
        member = message.author
        guild_id = member.guild.id

        db_connection, db_cursor = db.connect_to_db("data.db")

        guild_row = db_cursor.execute("SELECT join_leave_system, traffic_channel, leave_message, general_channel FROM servers WHERE guild_id = ?", (guild_id,)).fetchone()

        general_channel = guild_row["general_channel"]
        join_leave_system = guild_row["join_leave_system"]
        traffic_channel = guild_row["traffic_channel"]
        leave_message = guild_row["leave_message"]

        if general_channel is not None:
            general_channel = self.bot.get_channel(int(general_channel))

        if join_leave_system == 1:
            if traffic_channel is not None:
                traffic_channel = self.bot.get_channel(int(traffic_channel))

                if leave_message is not None:
                    leave_message = JoinLeave(self.bot).key_check(member, leave_message)
                    await traffic_channel.send(f"{leave_message}")
            else:
                await general_channel.send(":warning: **System Message**: The Join/Leave system has been turned on but there is no `traffic_channel` set.")

        db_connection.close()

async def setup(bot):
    log.in_log("INFO", "listener_setup", "Join/Leave System has been loaded")
    await bot.add_cog(JoinLeave(bot))
