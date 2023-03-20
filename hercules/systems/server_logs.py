import discord
from discord.ext import commands
import humanize
import datetime as dt
import sqlite3
import hercules.helper.log as log
import hercules.helper.herculesdb as db

class ServerLogs(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.Cog.listener()
    async def on_member_join(self, member):
        guild_id = member.guild.id

        db_connection, db_cursor = db.connect_to_db("data.db")

        row = db_cursor.execute("SELECT general_channel, logs_system, logs_channel FROM servers WHERE guild_id = ?", (guild_id,)).fetchone()
        general_channel = row["general_channel"]
        logs_system = row["logs_system"]
        logs_channel = row["logs_channel"]

        db_connection.close()

        if general_channel is not None:
            general_channel = self.bot.get_channel(int(general_channel))

        if logs_system == 1:
            if logs_channel is not None:
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
            else:
                await general_channel.send(":warning: **System Message**: The Logs System has been triggered by an event but there is no `logs_channel` set.")

    @commands.Cog.listener()
    async def on_member_remove(self, member):
        guild_id = member.guild.id

        db_connection, db_cursor = db.connect_to_db("data.db")

        row = db_cursor.execute("SELECT general_channel, logs_system, logs_channel FROM servers WHERE guild_id = ?", (guild_id,)).fetchone()
        general_channel = row["general_channel"]
        logs_system = row["logs_system"]
        logs_channel = row["logs_channel"]

        db_connection.close()

        if general_channel is not None:
            general_channel = self.bot.get_channel(int(general_channel))

        if logs_system == 1:
            if logs_channel is not None:
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
            else:
                await general_channel.send(":warning: **System Message**: The Logs System has been triggered by an event but there is no `logs_channel` set.")

    @commands.Cog.listener()
    async def on_member_update(self, before, after):
        member = before
        guild_id = member.guild.id

        db_connection, db_cursor = db.connect_to_db("data.db")

        row = db_cursor.execute("SELECT general_channel, logs_system, logs_channel FROM servers WHERE guild_id = ?", (guild_id,)).fetchone()
        general_channel = row["general_channel"]
        logs_system = row["logs_system"]
        logs_channel = row["logs_channel"]

        db_connection.close()

        if general_channel is not None:
            general_channel = self.bot.get_channel(int(general_channel))

        if logs_system == 1:
            if logs_channel is not None:
                logs_channel = self.bot.get_channel(int(logs_channel))

                member_tag = f"{member.name}#{member.discriminator}"
                member_id = member.id

                if not member.avatar:
                    member_pfp = member.default_avatar.url
                else:
                    member_pfp = member.avatar.url

                if not before.nick:
                    before_nick = "None"
                else:
                    before_nick = before.nick

                if not after.nick:
                    after_nick = "None"
                else:
                    after_nick = after.nick

                # Nickname changes
                if not before_nick == after_nick:
                    embed_content = {
                        "title": f":writing_hand: {member_tag}'s nickname was changed",
                        "fields": [
                            {"name": "Old nickname", "value": before_nick, "inline": True},
                            {"name": "New nickname", "value": after_nick, "inline": True}
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

                if len(before.roles) < len(after.roles): # roles must have been added
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
            else:
                await general_channel.send(":warning: **System Message**: The Logs System has been triggered by an event but there is no `logs_channel` set.")

    @commands.Cog.listener()
    async def on_member_ban(self, guild, user):
        guild_id = guild.id
        member = user

        db_connection, db_cursor = db.connect_to_db("data.db")

        row = db_cursor.execute("SELECT general_channel, logs_system, logs_channel FROM servers WHERE guild_id = ?", (guild_id,)).fetchone()
        general_channel = row["general_channel"]
        logs_system = row["logs_system"]
        logs_channel = row["logs_channel"]

        db_connection.close()

        if general_channel is not None:
            general_channel = self.bot.get_channel(int(general_channel))

        if logs_system == 1:
            if logs_channel is not None:
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
            else:
                await general_channel.send(":warning: **System Message**: The Logs System has been triggered by an event but there is no `logs_channel` set.")


    @commands.Cog.listener()
    async def on_message_delete(self, message):
        member = message.author
        channel = message.channel
        guild_id = message.guild.id

        db_connection, db_cursor = db.connect_to_db("data.db")

        row = db_cursor.execute("SELECT general_channel, logs_system, logs_channel FROM servers WHERE guild_id = ?", (guild_id,)).fetchone()
        general_channel = row["general_channel"]
        logs_system = row["logs_system"]
        logs_channel = row["logs_channel"]

        db_connection.close()

        if general_channel is not None:
            general_channel = self.bot.get_channel(int(general_channel))

        if logs_system == 1:
            if logs_channel is not None:
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
            else:
                await general_channel.send(":warning: **System Message**: The Logs System has been triggered by an event but there is no `logs_channel` set.")

async def setup(bot):
    log.in_log("INFO", "listener_setup", "Logs System has been loaded")
    await bot.add_cog(ServerLogs(bot))
