import discord
from discord.ext import commands
import humanize
import datetime as dt
import dateutil as du
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

        if not logs_channel == "null":
            logs_channel = self.bot.get_channel(int(logs_channel))

            member_tag = f"{member.name}#{member.discriminator}"
            member_created = member.created_at.strftime("%A, %d %b %Y at %l:%M%p")

            if member.default_avatar:
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

            await logs_channel.send(embed=embed)
