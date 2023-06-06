import discord
from discord.ext import commands
import datetime as dt
import hercules.helper.log as log
import hercules.helper.utils as herculesutils

class Info(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

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

    @commands.group()
    async def info(self, ctx):
        if ctx.invoked_subcommand:
            return

    # Guild part
    @info.group()
    async def guild(self, ctx):
        if ctx.invoked_subcommand:
            return

        guild_data = await Info(self.bot).get_guild_data(ctx.guild.id)

        await ctx.reply(embed=guild_data)

    @guild.command()
    async def query(self, ctx, arg):
        guild = self.bot.get_guild(int(arg))

        if not guild:
            await ctx.reply(":x: Guild not found.")
            return

        guild_data = await Info(self.bot).get_guild_data(guild.id)

        await ctx.reply(embed=guild_data)

    @guild.command()
    async def index(self, ctx):
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

    # User part
    async def get_user_data(self, user, guild):
        username = f"{user.name}#{user.discriminator}"
        user_creation_date = discord.utils.format_dt(user.created_at, style="R")

        user_id = user.id
        pfp = user.avatar.url
        mention = user.mention

        if not isinstance(user, discord.user.User):
            highest_role = user.top_role.name

            roles = len(user.roles)

            permissions = []
            if user.guild_permissions.administrator:
                permissions = ['Administrator']
            else:
                for k, v in iter(permissions):
                    if v:
                        permissions.append(f"{k.title().replace('_', ' ')}")

            permissions = ", ".join(permissions)

            seen_in = "\n".join([g.name for g in user.mutual_guilds])
        else:
            highest_role = "None"
            roles = "None"
            permissions = "None"
            seen_in = "None"

        embed_content = {
            "title": username,
            "fields": [
                { "name": "Creation date", "value": user_creation_date, "inline": False },
                { "name": "Mention", "value": mention, "inline": False },
                { "name": "Highest role", "value": highest_role, "inline": False },
                { "name": "Roles", "value": roles, "inline": False },
                { "name": "Permissions", "value": permissions, "inline": False },
                { "name": "Seen In", "value": seen_in, "inline": False }
            ]
        }

        embed = discord.Embed().from_dict(embed_content)
        embed.set_thumbnail(url=pfp)
        embed.set_footer(text=f"ID: {user_id}")

        return embed

    @info.group()
    async def user(self, ctx, arg=None):
        user = await herculesutils.handle_mention_or_id(self.bot, ctx, arg)
        user_data = await Info(self.bot).get_user_data(user, ctx.guild)

        await ctx.reply(embed=user_data)

async def setup(bot):
    log.in_log("INFO", "command_setup", "command info has been loaded")
    await bot.add_cog(Info(bot))
