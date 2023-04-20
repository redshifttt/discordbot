import discord
from discord.ext import commands
import datetime as dt
import hercules.helper.log as log

class GuildInfo(commands.Cog):
    def __init__(self, bot):
        self.bot = bot
        self.time_format = "%a, %b %d %Y %H:%M"

    async def get_guild_data(self, guild_id):
        guild = self.bot.get_guild(int(guild_id))

        guild_name = str(guild)
        guild_id = guild.id
        guild_owner = guild.owner.mention
        guild_icon = guild.icon

        creation_date = discord.utils.format_dt(guild.created_at, style="R")

        # Emojis
        emojis = guild.emojis
        emojis_count = str(len(emojis))

        # Members
        members = guild.member_count
        member_count = str(members)

        type_of_member = [member.bot for member in guild.members]
        regular_user = 0
        for mem_type in type_of_member:
            if not mem_type:
                regular_user += 1

        bot_user = members - regular_user

        member_count += f" ({regular_user} users, {bot_user} bots)"

        # Roles
        roles = guild.roles
        role_count = str(len(roles))
        role_count += f' ({", ".join([r.name for r in guild.roles])})'

        # Channels
        channels = guild.channels
        channel_count = str(len(channels))

        channel_breakdown = {}
        type_of_channel = [channel.type[0] for channel in guild.channels]

        for channel in type_of_channel:
            if channel not in channel_breakdown:
                channel_breakdown[channel] = 1
            else:
                channel_breakdown[channel] += 1

        channel_count = f"**{channel_count}** in total:\n"
        channel_count += "\n".join([f'- {v} {k}' for k,v in channel_breakdown.items()])

        embed_content = {
            "title": guild_name,
            "fields": [
                { "name": "Members", "value": member_count, "inline": False },
                { "name": "Owner", "value": guild_owner, "inline": False },
                { "name": "Created", "value": creation_date, "inline": False },
                { "name": "Roles", "value": role_count, "inline": False },
                { "name": "Channels", "value": channel_count, "inline": False },
                { "name": "Emotes", "value": emojis_count, "inline": False }
            ]
        }

        embed = discord.Embed().from_dict(embed_content)

        if guild_icon:
            embed.set_thumbnail(url=guild_icon.url)

        embed.set_footer(text=f'ID: {guild_id}')

        return embed

    @commands.group(aliases=["g", "server"])
    async def guild(self, ctx):
        if ctx.invoked_subcommand:
            return

        guild_data = await GuildInfo(self.bot).get_guild_data(ctx.guild.id)

        await ctx.reply(embed=guild_data)

    @guild.command(aliases=["q"])
    async def query(self, ctx, arg):
        guild = self.bot.get_guild(int(arg))

        if not guild:
            await ctx.reply(":x: Guild not found.")
            return

        guild_data = await GuildInfo(self.bot).get_guild_data(ctx.guild.id)

        await ctx.reply(embed=guild_data)

    @guild.command(aliases=["a", "list", "l"])
    async def all(self, ctx):
        guilds = self.bot.guilds

        guild_count = len(guilds)
        user_count = sum([g.member_count for g in guilds])
        channel_count = sum([len(g.channels) for g in guilds])

        guilds_stats = f"__**Overall Statistics:**__ **{user_count}** users across **{guild_count}** servers. **{channel_count}** channels watched simultaneously.\n"

        for guild in guilds:
            guild_id = guild.id
            guild_name = guild.name
            guild_member_count = guild.member_count
            guild_creation_date = discord.utils.format_dt(guild.created_at, style="R")

            guilds_stats += f"`{guild_id}` **{guild_name}** ({guild_member_count} members, created {guild_creation_date})\n"

        sorted(guilds_stats)
        await ctx.reply(guilds_stats)

async def setup(bot):
    log.in_log("INFO", "command_setup", "command guild has been loaded")
    await bot.add_cog(GuildInfo(bot))
