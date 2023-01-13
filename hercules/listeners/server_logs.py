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

    @commands.Cog.listener(name="on_member_remove")
    async def server_log_member_leave(self, member):
        guild_id = member.guild.id

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

    @commands.Cog.listener(name="on_member_update")
    async def server_log_member_update(self, before, after):
        member = before
        guild_id = member.guild.id

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()

        logs_channel = db_cur.execute("SELECT logs_channel FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]

        db_con.close()

        if not logs_channel == "null":
            logs_channel = self.bot.get_channel(int(logs_channel))

            member_tag = f"{member.name}#{member.discriminator}"
            member_id = member.id

            if not member.avatar:
                member_pfp = member.default_avatar.url
            else:
                member_pfp = member.avatar.url

            # Nickname changes
            if not before.nick == after.nick:
                embed_content = {
                    "title": f":writing_hand: {member_tag}'s nickname was changed",
                    "fields": [
                        {"name": "Old nickname", "value": before.nick, "inline": True},
                        {"name": "New nickname", "value": after.nick, "inline": True}
                    ]
                }

                embed = discord.Embed().from_dict(embed_content)
                embed.set_thumbnail(url=member_pfp)
                embed.set_footer(text=f"User ID: {member_id}")

                await logs_channel.send(embed=embed)

            if len(before.roles) > len(after.roles): # roles must have been removed
                removed_roles = list(set(before.roles).difference(after.roles))
                removed_roles = ", ".join([role.name for role in removed_roles])
                embed_content = {
                    "title": f":scroll: {member_tag} had roles removed",
                    "fields": [
                        {"name": "Removed roles", "value": removed_roles, "inline": True},
                    ]
                }

                embed = discord.Embed().from_dict(embed_content)
                embed.set_thumbnail(url=member_pfp)
                embed.set_footer(text=f"User ID: {member_id}")

                await logs_channel.send(embed=embed)
            else:
                added_roles = list(set(after.roles).difference(before.roles))
                added_roles = ", ".join([role.name for role in added_roles])
                embed_content = {
                    "title": f":scroll: {member_tag} had roles added",
                    "fields": [
                        {"name": "Added roles", "value": added_roles, "inline": True},
                    ]
                }

                embed = discord.Embed().from_dict(embed_content)
                embed.set_thumbnail(url=member_pfp)
                embed.set_footer(text=f"User ID: {member_id}")

                await logs_channel.send(embed=embed)

    @commands.Cog.listener(name="on_member_ban")
    async def server_log_member_join(self, guild, user):
        guild_id = guild.id
        member = user

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()

        logs_channel = db_cur.execute("SELECT logs_channel FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]

        db_con.close()

        if not logs_channel == "null":
            logs_channel = self.bot.get_channel(int(logs_channel))

            member_tag = f"{member.name}#{member.discriminator}"
            member_id = member.id
            member_joined_at = member.joined_at.strftime("%A, %e %b %Y at %l:%M%p")

            if not member.avatar:
                member_pfp = member.default_avatar.url
            else:
                member_pfp = member.avatar.url

            embed_content = {
                "title": f":hammer: {member_tag} has been banned!",
                "fields": [
                    {"name": "Joined the server on", "value": member_joined_at, "inline": True},
                ]
            }

            embed = discord.Embed().from_dict(embed_content)
            embed.set_thumbnail(url=member_pfp)
            embed.set_footer(text=f"User ID: {member_id}")

            await logs_channel.send(embed=embed)

    @commands.Cog.listener(name="on_message_delete")
    async def server_log_member_join(self, message):
        member = message.author
        channel = message.channel
        guild_id = message.guild.id

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()

        logs_channel = db_cur.execute("SELECT logs_channel FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]

        db_con.close()

        if not logs_channel == "null":
            logs_channel = self.bot.get_channel(int(logs_channel))

            member_tag = f"{member.name}#{member.discriminator}"
            member_id = member.id
            member_joined_at = member.joined_at.strftime("%A, %e %b %Y at %l:%M%p")

            if not member.avatar:
                member_pfp = member.default_avatar.url
            else:
                member_pfp = member.avatar.url

            embed_content = {
                "title": f":thumbsdown: {member_tag}'s message got deleted in #{channel.name}",
                "fields": [
                    {"name": "Deleted message", "value": message.clean_content, "inline": False},
                    {"name": "Channel", "value": channel.mention, "inline": False},
                ]
            }

            embed = discord.Embed().from_dict(embed_content)
            embed.set_thumbnail(url=member_pfp)
            embed.set_footer(text=f"User ID: {member_id}")

            await logs_channel.send(embed=embed)
