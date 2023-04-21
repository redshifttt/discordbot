import discord
from discord.ext import commands
import hercules.helper.log as log
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
    log.in_log("INFO", "command_setup", "command avatar has been loaded")
    await bot.add_cog(Avatar(bot))
