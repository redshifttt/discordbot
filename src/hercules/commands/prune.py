import discord
from discord.ext import commands
import random
import hercules.helper.log as log

class Prune(commands.Cog):
    def __init__(self, bot):
        self.bot = bot
        self.setup = self.bot

    @commands.command()
    async def prune(self, ctx, *args):
        if not args:
            return
        if not ctx.author.guild_permissions.manage_messages:
            await ctx.reply("nice try")
            return

        channel = ctx.channel
        amount = int(args[0]) + 1
        messages_to_delete = []

        async for m in channel.history(limit=amount):
            messages_to_delete.append(m)

        await channel.delete_messages(messages_to_delete)

async def setup(bot):
    log.in_log("INFO", "command_setup", "command prune has been loaded")
    await bot.add_cog(Prune(bot))
