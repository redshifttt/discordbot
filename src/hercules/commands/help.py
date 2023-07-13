import discord
from discord.ext import commands

class Help(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.command(name="help", brief="Show this help menu", aliases=["h"])
    async def help(self, ctx):
        await ctx.reply(":grey_question: Get the full help documentation here: https://soda.privatevoid.net/num/bot/")

async def setup(bot):
    await bot.add_cog(Help(bot))
