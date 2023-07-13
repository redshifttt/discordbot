import discord
from discord.ext import commands
import hercules.helper.utils as herculesutils

class Avatar(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.command(aliases=["avi", "pfp"])
    async def avatar(self, ctx, arg=None):
        user = await herculesutils.handle_mention_or_id(self.bot, ctx, arg)

        embed = discord.Embed(title=f"{user.name}'s profile picture")
        embed.set_image(url=user.avatar.url)

        await ctx.reply(embed=embed)

async def setup(bot):
    await bot.add_cog(Avatar(bot))
