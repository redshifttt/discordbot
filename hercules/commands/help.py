import discord
from discord.ext import commands
import hercules.helper.log as log

class Help(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.command(name="help", brief="Show this help menu", aliases=["h"])
    async def help(self, ctx):
        await ctx.reply(":grey_question: Get the full help documentation here: https://soda.privatevoid.net/num/bot/")

async def setup(bot):
    log.in_log("INFO", "command_setup", "command help has been loaded")
    await bot.add_cog(Help(bot))
