import discord
from discord.ext import commands
import datetime as dt

class GuildInfo(commands.Cog):
    def __init__(self, bot):
        self.bot = bot
        self.time_format = "%a, %b %d %Y %H:%M"

    @commands.group(
        name="guild",
        brief="Gets information about the current server or others",
        aliases=["g", "server"]
    )
    async def guild(self, ctx):
        if ctx.invoked_subcommand:
            return

        guild = ctx.guild

        server_name = guild.name
        server_id = guild.id
        server_owner = guild.owner.mention
        server_icon = guild.icon.url

        creation_date = guild.created_at
        creation_date_relative = discord.utils.format_dt(creation_date, style="R")

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

        channel_count = str(len(guild.channels))
        channel_type = {}

        type_of_channel = [channel.type[0] for channel in guild.channels]

        for channel in type_of_channel:
            if channel not in channel_type:
                channel_type[channel] = 1
            else:
                channel_type[channel] += 1

        channel_count = f"**{channel_count}**\n"
        channel_count += "\n".join([f'*{v} {k}*' for k,v in channel_type.items()])

        embed_content = {
            "title": server_name,
            "fields": [
                { "name": "Members", "value": members, "inline": False },
                { "name": "Owner", "value": f"{server_owner}", "inline": False },
                { "name": "Created", "value": f"{creation_date_relative}", "inline": False },
                { "name": "Roles", "value": roles, "inline": True },
                { "name": "Channels", "value": channel_count, "inline": True },
                { "name": "Emotes", "value": emote_count, "inline": True }
            ]
        }

        embed = discord.Embed().from_dict(embed_content)
        embed.set_thumbnail(url=server_icon)
        embed.set_footer(text=f"ID: {server_id}")

        await ctx.reply(embed=embed)

    @guild.command(
        help="Get a list of all the servers the bot is in",
        aliases=["a", "list", "l"]
    )
    async def all(self, ctx):
        guilds = self.bot.guilds
        guild_count = len(guilds)
        guild_list = f"{guild_count} servers\n"
        for guild in guilds:
            guild_id = guild.id
            guild_name = guild.name
            guild_member_count = guild.member_count

            guild_list += f"`{guild_id}` {guild_name} ({guild_member_count} members)\n"

        sorted(guild_list)
        await ctx.reply(guild_list)

    @guild.command(
        help="Query information about a guild from its ID",
        aliases=["q"]
    )
    async def query(self, ctx, arg):
        guild = self.bot.get_guild(int(arg))

        server_name = guild.name
        server_id = guild.id
        server_owner = guild.owner.mention
        server_icon = guild.icon.url

        creation_date = guild.created_at
        creation_date_relative = discord.utils.format_dt(creation_date, style="R")

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

        channel_count = str(len(guild.channels))
        channel_type = {}

        type_of_channel = [channel.type[0] for channel in guild.channels]

        for channel in type_of_channel:
            if channel not in channel_type:
                channel_type[channel] = 1
            else:
                channel_type[channel] += 1

        channel_count = f"**{channel_count}**\n"
        channel_count += "\n".join([f'*{v} {k}*' for k,v in channel_type.items()])

        embed_content = {
            "title": server_name,
            "fields": [
                { "name": "Members", "value": members, "inline": False },
                { "name": "Owner", "value": f"{server_owner}", "inline": False },
                { "name": "Created", "value": f"{creation_date_relative}", "inline": False },
                { "name": "Roles", "value": roles, "inline": True },
                { "name": "Channels", "value": channel_count, "inline": True },
                { "name": "Emotes", "value": emote_count, "inline": True }
            ]
        }

        embed = discord.Embed().from_dict(embed_content)
        embed.set_thumbnail(url=server_icon)
        embed.set_footer(text=f"ID: {server_id}")

        await ctx.reply(embed=embed)
