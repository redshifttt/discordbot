import discord
from discord.ext import commands
import datetime as dt

class ServerInfo(commands.Cog):
    def __init__(self, bot):
        self.bot = bot
        self.time_format = "%a, %b %d %Y %H:%M"

    @commands.command(name="serverinfo", brief="Gets information about the server", aliases=["si"])
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
                { "name": "Created", "value": f"{creation_date}", "inline": False },
                { "name": "Roles", "value": roles, "inline": False },
                { "name": "Channels", "value": channel_count, "inline": False },
                { "name": "Emotes", "value": emote_count, "inline": False }
            ]
        }

        embed = discord.Embed().from_dict(embed_content)
        embed.set_thumbnail(url=server_icon)
        embed.set_footer(text=f"ID: {server_id}")

        await ctx.reply(embed=embed)

