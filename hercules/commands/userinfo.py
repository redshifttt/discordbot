import discord
from discord.ext import commands

class UserInfo(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.command(
        name="userinfo",
        brief="Gets information about a user",
        help="Gets user information for a user or yourself.\n\n**Usage**\n`.userinfo`\n`.userinfo @user`",
        aliases=["u"]
    )
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

