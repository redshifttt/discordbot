import discord
from discord.ext import commands

class Avatar(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.command(
        name="avatar",
        brief="Get the profile picture of a user or yourself",
        help="Gets the profile picture of a user or yourself.\n\n**Usage**\n`.avatar`\n`.avatar @user`",
        aliases=["avi", "pfp"]
    )
    async def avatar(self, ctx):
        if not ctx.message.mentions:
            user = ctx.author
        else:
            user = ctx.message.mentions[0]

        embed = discord.Embed(title=f"{user.name}'s profile picture")
        embed.set_image(url=user.avatar.url)

        await ctx.reply(embed=embed)
