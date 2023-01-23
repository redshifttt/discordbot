import discord
from discord.ext import commands
import sqlite3
import hercules.helper.log as log

class JoinLeave(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.Cog.listener(name='on_member_join')
    async def member_join(self, member):
        guild_id = member.guild.id

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()

        general_channel = db_cur.execute("SELECT general_channel FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]
        join_leave_system = db_cur.execute("SELECT join_leave_system FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]
        traffic_channel = db_cur.execute("SELECT traffic_channel FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]
        join_message = db_cur.execute("SELECT join_message FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]

        verification_system = db_cur.execute("SELECT verification_system FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]
        verification_channel = db_cur.execute("SELECT verification_channel FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]
        verification_message = db_cur.execute("SELECT verification_message FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]

        if not join_leave_system == "null":
            # Finding the channels
            if not traffic_channel == "null":
                traffic_channel = self.bot.get_channel(int(traffic_channel))

                if not join_message == "null":
                    await traffic_channel.send(f":inbox_tray: {member.mention} {join_message}")
            else:
                general_channel = self.bot.get_channel(int(general_channel))
                await general_channel.send(":warning: **System Message**: The Join/Leave system has been turned on but there is no `traffic_channel` set.")

        if not verification_system == "null":
            if not verification_channel == "null":
                if not verification_message == "null":
                    verification_channel = self.bot.get_channel(int(verification_channel))
                    await verification_channel.send(f"{member.mention} {verification_message}")
            else:
                general_channel = self.bot.get_channel(int(general_channel))
                await general_channel.send(":warning: **System Message**: The Verification System has been turned on but there is no `verification_channel` set.")

        db_con.close()

    @commands.Cog.listener(name='on_member_remove')
    async def member_leave(self, member):
        guild_id = member.guild.id

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()

        join_leave_system = db_cur.execute("SELECT join_leave_system FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]
        traffic_channel = db_cur.execute("SELECT traffic_channel FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]
        leave_message = db_cur.execute("SELECT leave_message FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]
        general_channel = db_cur.execute("SELECT general_channel FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]

        if not join_leave_system == "null":
            if not traffic_channel == "null":
                if not leave_message == "null":
                    traffic_channel = self.bot.get_channel(int(traffic_channel))
                    await traffic_channel.send(f":outbox_tray: {member.mention} {leave_message}")
            else:
                general_channel = self.bot.get_channel(int(general_channel))
                await general_channel.send(":warning: **System Message**: The Join/Leave system has been turned on but there is no `traffic_channel` set.")


        db_con.close()

async def setup(bot):
    log.in_log("INFO", "listener_setup", "Join/Leave System has been loaded")
    await bot.add_cog(JoinLeave(bot))
