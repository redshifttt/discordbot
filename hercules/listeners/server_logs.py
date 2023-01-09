import discord
from discord.ext import commands
import humanize
import datetime as dt
import sqlite3

class ServerEventLogging(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.Cog.listener(name="on_member_join")
    async def server_log_member_join(self, member):
        guild_id = member.guild.id

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()

        logs_channel = db_cur.execute("SELECT logs_channel FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]

        db_con.close()

        if not logs_channel == "null":
            logs_channel = self.bot.get_channel(int(logs_channel))

            member_tag = f"{member.name}#{member.discriminator}"
            member_created = member.created_at.strftime("%A, %e %b %Y at %l:%M%p")
            member_id = member.id

            if not member.avatar:
                member_pfp = member.default_avatar.url
            else:
                member_pfp = member.avatar.url

            member_mutual_guilds = "\n".join([f"- {guild.name}" for guild in member.mutual_guilds])
            member_mutual_guilds_count = len(member.mutual_guilds)

            embed_content = {
                "title": f":inbox_tray: {member_tag} joined the server.",
                "fields": [
                    {"name": "Account created", "value": member_created, "inline": True},
                    {"name": "Mutual guilds with bot", "value": f"**{member_mutual_guilds_count}**\n{member_mutual_guilds}", "inline": False}
                ]
            }

            embed = discord.Embed().from_dict(embed_content)
            embed.colour = discord.Color.from_str("#47D63F")
            embed.set_thumbnail(url=member_pfp)
            embed.set_footer(text=f"User ID: {member_id}")

            await logs_channel.send(embed=embed)

    @commands.Cog.listener(name="on_member_remove") ## CHANGE THIS
    async def server_log_member_leave(self, member): ## CHANGE THIS
        guild_id = member.guild.id ## CHANGE MESSAGE TO MEMBER

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()

        logs_channel = db_cur.execute("SELECT logs_channel FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]

        db_con.close()

        if not logs_channel == "null":
            logs_channel = self.bot.get_channel(int(logs_channel))

            member_tag = f"{member.name}#{member.discriminator}"
            member_joined_at = member.joined_at.strftime("%A, %e %b %Y at %l:%M%p")
            member_id = member.id

            if not member.avatar:
                member_pfp = member.default_avatar.url
            else:
                member_pfp = member.avatar.url

            embed_content = {
                "title": f":outbox_tray: {member_tag} left the server.",
                "fields": [
                    {"name": "Joined the server on", "value": member_joined_at, "inline": True},
                ]
            }

            embed = discord.Embed().from_dict(embed_content)
            embed.colour = discord.Color.red()
            embed.set_thumbnail(url=member_pfp)
            embed.set_footer(text=f"User ID: {member_id}")

            await logs_channel.send(embed=embed)
