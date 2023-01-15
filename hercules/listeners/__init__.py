import discord
from discord.ext import commands
import aiohttp
import datetime as dt
import sqlite3

class GenericListeners(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.Cog.listener(name='on_member_join')
    async def member_join(self, member):
        guild_id = member.guild.id

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()

        traffic_channel = db_cur.execute("SELECT traffic_channel FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]
        join_message = db_cur.execute("SELECT join_message FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]

        verification_channel = db_cur.execute("SELECT verification_channel FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]
        verification_message = db_cur.execute("SELECT verification_message FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]

        # Finding the channels
        if not traffic_channel == "null":
            traffic_channel = self.bot.get_channel(int(traffic_channel))

            if not join_message == "null":
                await traffic_channel.send(f":inbox_tray: {member.mention} {join_message}")

        if not verification_channel == "null":
            if not verification_message == "null":
                verification_channel = self.bot.get_channel(int(verification_channel))
                await verification_channel.send(f"{member.mention} {verification_message}")

        db_con.close()

    @commands.Cog.listener(name='on_member_remove')
    async def member_leave(self, member):
        guild_id = member.guild.id

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()

        traffic_channel = db_cur.execute("SELECT traffic_channel FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]
        leave_message = db_cur.execute("SELECT leave_message FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]

        if not traffic_channel == "null":
            if not leave_message == "null":
                traffic_channel = self.bot.get_channel(int(traffic_channel))
                await traffic_channel.send(f":outbox_tray: {member.mention} {leave_message}")

        db_con.close()

    @commands.Cog.listener(name='on_message')
    async def message_link_resolve(self, message):
        if "discord.com/channels" not in message.content:
            return

        split_content = message.content.split()

        for val in split_content:
            if "/channels" in val:
                link = val

        *_, channel_id, message_id = link.split("/")

        found_channel = message.guild.get_channel(int(channel_id))
        found_message = await found_channel.fetch_message(int(message_id))

        user_name = found_message.author.name
        content = found_message.clean_content

        if found_message.embeds:
            content = "**`Cannot have an embed inside and embed.`**"

        embed = discord.Embed(title=f"{user_name}'s message", description=content)

        embed.set_author(name=message.guild.name, icon_url=message.guild.icon.url)
        embed.set_footer(text=f"Linked by {message.author.name}", icon_url=message.author.avatar.url)
        embed.add_field(name="Jump to message", value=f"[here]({link})")

        await message.reply(embed=embed)

    @commands.Cog.listener(name='on_message')
    async def invite_nuker(self, message):
        if "discord.gg" in message.content:
            await message.delete()
            await message.author.timeout(dt.timedelta(minutes=1), reason=f"Advertising in #{message.channel.name}")

    @commands.Cog.listener(name='on_guild_join')
    async def db_server_init(self, guild):
        guild_id = guild.id

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()

        db_cur.execute("INSERT INTO servers VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?)", (guild_id, null, null, null, null, null, null, null, null,))
        db_con.commit()

