import discord
from discord.ext import commands
import hercules.helper.log as log

class UserInfo(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.command(
        name="user",
        brief="Gets information about a user",
        help="Gets user information for a user or yourself.\n\n**Usage**\n`./user`\n`./user @user`\n`./user 356830440629207040`",
        aliases=["u"]
    )
    async def userinfo(self, ctx, arg=None):
        embed_content = {}

        guild = ctx.guild

        if not arg:
            user = ctx.author
        else:
            if arg.isnumeric():
                user = ctx.guild.get_member(int(arg)) or await self.bot.fetch_user(int(arg))
                if not user:
                    await ctx.reply("Not a valid user ID")
                    return
            if arg.startswith("<@"):
                user = ctx.guild.get_member(int(arg[2:-1]))

        guild_name = guild.name
        guild_icon = guild.icon
        print(guild_icon)

        username = f"{user.name}#{user.discriminator}"
        user_creation = discord.utils.format_dt(user.created_at, style="R")

        embed_content["title"] = username
        embed_content["fields"] = []
        embed_content["fields"].append({ "name": "Creation date", "value": user_creation, "inline": False },)

        user_id = user.id
        pfp = user.avatar.url
        mention = user.mention
        embed_content["fields"].append({ "name": "Mention", "value": mention, "inline": False })

        # Fucking exceptions
        try:
            if user.roles:
                user_roles = user.roles
                roles = len(user_roles)
                embed_content["fields"].append({ "name": "Roles", "value": roles, "inline": False })
        except:
            pass

        try:
            if user.top_role:
                highest_role = user.top_role.name
                embed_content["fields"].append({ "name": "Highest role", "value": highest_role, "inline": False })
        except:
            pass

        try:
            if user.guild_permissions:
                permissions = user.guild_permissions
                perms = []
                if permissions.administrator:
                    perms = ['Administrator']
                else:
                    for k, v in iter(permissions):
                        if v:
                            perms.append(f"{k.title().replace('_', ' ')}")

                perms = ", ".join(perms)
                embed_content["fields"].append({ "name": "Permissions", "value": perms, "inline": False })
        except:
            pass

        embed = discord.Embed().from_dict(embed_content)
        embed.set_thumbnail(url=pfp)
        embed.set_footer(text=f"ID: {user_id}")

        if guild_icon:
            embed.set_author(name=guild_name, icon_url=guild_icon.url)
        else:
            embed.set_author(name=guild_name)

        await ctx.reply(embed=embed)


async def setup(bot):
    log.in_log("INFO", "command_setup", "command user has been loaded")
    await bot.add_cog(UserInfo(bot))
