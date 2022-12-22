import discord
from discord.ext import commands
import humanize
import datetime as dt

class Christmas(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.command(name="christmas")
    async def christmas(self, ctx):
        christmas = humanize.precisedelta(dt.datetime(2022, 12, 25, 0, 0, 0, 0) - dt.datetime.today(), minimum_unit="hours", format="%.0f")
        if dt.datetime.today().month == 12:
            reply = f"{christmas} till Christmas!"
        else:
            reply = "it's already christmas dummy!"

        await ctx.reply(reply)

