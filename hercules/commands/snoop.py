import discord
from discord.ext import commands
import hercules.helper.log as log
import hercules.helper.utils as herculesutils
import time

class Snoop(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    # async def get_channel_messages(self, channel_id):

    @commands.group()
    async def snoop(self, ctx):
        if ctx.invoked_subcommand:
            return
        await ctx.reply(";)")

    @snoop.group()
    async def guild(self, ctx, guild_arg):
        guild = self.bot.get_guild(int(guild_arg))
        channels = guild.channels
        categories = guild.categories
        channels_list = ""

        channels_list = f"**__Channels list for {guild.name}__**\n"
        for cat in categories:
            channels_list += f"**→ {cat.name}**\n"
            for channel in cat.channels:
                channels_list += "\t"
                channels_list += f"{channel.name} `{channel.id}`"
                channels_list += "\n"

        await ctx.reply(channels_list)

    @snoop.group()
    async def channel(self, ctx, channel_arg):
        channel = self.bot.get_channel(int(channel_arg))

        log = ""
        messages = []
        async for message in channel.history(limit=500):
            author = message.author
            content = message.content
            created_at = discord.utils.format_dt(message.created_at, style="R")
            messages.append(f"{created_at} **{author}**: {content}\n")

        messages.reverse()
        for m in messages:
            log += m

        if not len(log) >= 4000:
            with open(f"Hercules messages for {channel.name}.txt", "w") as f:
                f.write(log)
            await ctx.reply(file=discord.File(f"Hercules messages for {channel.name}.txt"))
        else:
            await ctx.reply(log)

async def setup(bot):
    log.in_log("INFO", "command_setup", "command snoop has been loaded")
    await bot.add_cog(Snoop(bot))
