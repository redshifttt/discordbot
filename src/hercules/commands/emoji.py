import discord
from discord.ext import commands
import requests

class Emoji(commands.Cog):
    def __init__(self, bot):
        self.bot = bot
        self.setup = self.bot

    @commands.command()
    async def emoji(self, ctx, *args):
        if not len(args) == 2:
            await ctx.reply(":x: Please provide a name for the emoji and a link. Example: `./emoji <name> <link>`.")
            return

        emoji_name, emoji_link = args

        if not emoji_link.startswith("http"):
            await ctx.reply(":x: Not a valid link.")
            return

        res = requests.get(emoji_link)

        if not res.status_code == 200:
            await ctx.reply(f":x: Link returns error code `{res.status_code}`.")
            return

        await ctx.guild.create_custom_emoji(name=emoji_name, image=res.content)
        await ctx.reply(f":white_check_mark: Created emoji `{emoji_name}`.")

async def setup(bot):
    await bot.add_cog(Emoji(bot))
