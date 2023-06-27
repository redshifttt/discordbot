import discord
from discord.ext import commands
import random
import hercules.helper.log as log

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
    log.in_log("INFO", "command_setup", "command ask has been loaded")
    await bot.add_cog(Ask(bot))
