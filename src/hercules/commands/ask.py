import discord
from discord.ext import commands
import random

class Ask(commands.Cog):
    def __init__(self, bot):
        self.bot = bot
        self.setup = self.bot

    @commands.command()
    async def ask(self, ctx, *args):
        args = " ".join(args)
        if not "or" in args:
            return

        fortune = args.split("or")
        fortune = [f.strip() for f in fortune]
        fortune = random.choice(fortune)

        await ctx.reply(fortune)

async def setup(bot):
    await bot.add_cog(Ask(bot))
