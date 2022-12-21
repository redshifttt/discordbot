import discord
from discord.ext import commands
import datetime as dt
import time
import requests
import humanize
import random
import json
import sqlite3

class User(commands.Cog):
    def __init__(self, bot):
        self.bot = bot
        self.time_format = "%a, %b %d %Y %H:%M"

    @commands.command(name="serverinfo")
    async def serverinfo(self, ctx):
        guild = ctx.guild

        server_name = guild.name
        server_id = guild.id
        server_owner = guild.owner.mention
        server_icon = guild.icon.url

        creation_date = guild.created_at.strftime(self.time_format)

        emote_count = len(guild.emojis)

        members = str(guild.member_count)
        type_of_member = [member.bot for member in guild.members]

        regular_user = 0
        bot_user = 0
        for mem_type in type_of_member:
            if not mem_type:
                regular_user += 1
            else:
                bot_user += 1

        members += f" ({regular_user} users, {bot_user} bots)"

        roles = len(guild.roles)

        channels = str(len(guild.channels))
        channel_type = {}

        for channel in guild.channels:
            type_of_channel = channel.type[0]
            if type_of_channel not in channel_type:
                channel_type[type_of_channel] = 1
            else:
                channel_type[type_of_channel] += 1

        channels += f" ({', '.join([f'{v} {k}' for k,v in channel_type.items()])})"

        embed_content = {
            "title": server_name,
            "fields": [
                {
                    "name": "Members",
                    "value": members,
                    "inline": False
                },
                {
                    "name": "Owner",
                    "value": f"{server_owner}",
                    "inline": False
                },
                {
                    "name": "Created",
                    "value": f"{creation_date}",
                    "inline": False
                },
                {
                    "name": "Roles",
                    "value": roles,
                    "inline": False
                },
                {
                    "name": "Channels",
                    "value": channels,
                    "inline": False
                },
                {
                    "name": "Emotes",
                    "value": emote_count,
                    "inline": False
                }
            ]
        }

        embed = discord.Embed().from_dict(embed_content)
        embed.set_thumbnail(url=server_icon)
        embed.set_footer(text=f"ID: {server_id}")

        await ctx.reply(embed=embed)

    @commands.command(name="userinfo")
    async def userinfo(self, ctx):
        if not ctx.message.mentions:
            user = ctx.author
        else:
            user = ctx.message.mentions[0]

        current_guild = ctx.guild
        guild_name = current_guild.name
        guild_icon = current_guild.icon.url

        username = user.name
        user_id = user.id
        mention = user.mention
        status = user.raw_status

        pfp = user.avatar.url

        user_roles = user.roles
        mention = user.mention
        highest_role = user.top_role.name
        permissions = user.guild_permissions

        perms = []
        if permissions.administrator:
            perms = ['Administrator']
        else:
            for k, v in iter(permissions):
                if v:
                    perms.append(f"{k.title().replace('_', ' ')}")

        perms = ", ".join(perms)

        roles = len(user_roles)

        embed_content = {
            "title": username,
            "fields": [
                { "name": "Roles", "value": roles, "inline": False },
                { "name": "Highest role", "value": highest_role, "inline": False },
                { "name": "Permissions", "value": perms, "inline": False },
                { "name": "Mention", "value": mention, "inline": False },
            ]
        }

        embed = discord.Embed().from_dict(embed_content)
        embed.set_thumbnail(url=pfp)
        embed.set_author(name=current_guild, icon_url=guild_icon)
        embed.set_footer(text=f"ID: {user_id}")

        await ctx.reply(embed=embed)

    @commands.command(name="search")
    async def search(self, ctx, *args):
        async with self.bot.get_channel(ctx.channel.id).typing():
            # Turn command arguments (tuple) into a list
            search_term = list(args)
            url = f"https://search.privatevoid.net/search?q={search_term}&format=json"
            url_search_term = f"https://search.privatevoid.net/search?q={'+'.join(search_term)}"

            results = json.loads(
                requests.get(f"https://search.privatevoid.net/search?q={search_term}&format=json").text
            )["results"][:5]

            embed = discord.Embed(title=f"First 5 search results for \"{' '.join(search_term)}\"", url=url_search_term)

            for _, r in enumerate(results, 1):
                site_title = f"┃ {r['title']}"
                site_url = r["url"]
                site_description = f"{site_url}\n{r['content'][:150]}..."

                embed.add_field(name=site_title, value=site_description, inline=False)

            embed.set_author(name=f"{ctx.author.name}'s query", icon_url=ctx.author.avatar.url)
            embed.set_footer(
                text="All search queries are private | search.privatevoid.net",
                icon_url="https://cdn.privatevoid.net/private-void/branding/2022/hexagon-small-white-outline.png"
            )
            await ctx.reply(embed=embed)

    @commands.command(name="avatar")
    async def avatar(self, ctx):
        if not ctx.message.mentions:
            user = ctx.author
        else:
            user = ctx.message.mentions[0]

        embed = discord.Embed(title=f"{user.name}'s profile picture")
        embed.set_image(url=user.avatar.url)

        await ctx.reply(embed=embed)


    @commands.command(name="christmas")
    async def christmas(self, ctx):
        christmas = humanize.precisedelta(dt.datetime(2022, 12, 25, 0, 0, 0, 0) - dt.datetime.today(), minimum_unit="hours", format="%.0f")
        if dt.datetime.today().month == 12:
            reply = f"{christmas} till Christmas!"
        else:
            reply = "it's already christmas dummy!"

        await ctx.reply(reply)

    @commands.command(name="settings")
    async def settings(self, ctx, *args):
        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()
        guild_id = ctx.guild.id

        if not args:
            guild_id, traffic_channel, pins_channel, verification_channel, general_channel, logs_channel, join_message, leave_message, verification_message \
                = db_cur.execute("SELECT * FROM servers WHERE guild_id=?", (guild_id,)).fetchall()[0]

            embed_content = {
                "title": f"Hercules settings for {ctx.guild.name}",
                "fields": [
                    { "name": "Server ID", "value": guild_id, "inline": False },
                    { "name": "Traffic Channel", "value": traffic_channel, "inline": False },
                    { "name": "Pins Channel", "value": pins_channel, "inline": False },
                    { "name": "Verification Channel", "value": verification_channel, "inline": False },
                    { "name": "General Channel", "value": general_channel, "inline": False },
                    { "name": "Logs Channel", "value": logs_channel, "inline": False },
                    { "name": "Join Message", "value": join_message, "inline": False },
                    { "name": "Leave Message", "value": leave_message, "inline": False },
                    { "name": "Verification Message", "value": verification_message, "inline": False }
                ]
            }

            embed = discord.Embed().from_dict(embed_content)
            embed.set_thumbnail(url=ctx.guild.icon.url)

            await ctx.reply(embed=embed)
            return

        if not len(args) == 2:
            await ctx.reply(":x: wrong number of arguments")
            return

        header, value = args

        if header == "guild_id":
            await ctx.reply(f":x: Cannot change the current server ID")
            return

        res = db_cur.execute("SELECT guild_id FROM servers WHERE guild_id=?", (guild_id,))

        # Unfortunately the below doesn't work. Time to open myself up to SQL
        # injections - not like it matters though.
        # res = db_cur.execute("UPDATE servers SET ? = ? WHERE guild_id=?", (header, value, guild_id,))

        # This is what seems to be the best you can do when it comes to dynamic settings of values
        res = db_cur.execute(f"UPDATE servers SET {header} = '{value}' WHERE guild_id={guild_id}")

        if not res:
            await ctx.reply(f":x: There was an error setting that value.")
            return

        await ctx.reply(f":white_check_mark: {header} set to `{value}`")
        db_con.commit()
        db_con.close()
