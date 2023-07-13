import discord
from discord.ext import commands
import requests
import json

class Image(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.command(aliases=["img"])
    async def image(self, ctx, *args):
        message = await ctx.reply(":repeat: Getting image results. This may take about 5 seconds.")
        async with self.bot.get_channel(ctx.channel.id).typing():
            search_term = args

            url = f"https://search.privatevoid.net/search?q={'+'.join(search_term)}&category_images=on&format=json"

            results = json.loads(requests.get(url).text)["results"]
            total_results = len(results)

            results = results[:1][0]

            image_title = results['title']
            image_url = results["url"]
            image_link = results["img_src"]

            embed = discord.Embed(title=f"1/{total_results} image results for \"{' '.join(search_term)}\"")
            embed.description = f"[All images]({url.replace('&category_images=on&format=json','')})"

            embed.set_image(url=image_link)
            embed.add_field(name=image_title, value=image_url, inline=False)

            embed.set_footer(
                text="All search queries are private | search.privatevoid.net",
                icon_url="https://cdn.privatevoid.net/private-void/branding/2022/hexagon-small-white-outline.png"
            )

            await message.edit(content=None, embed=embed)

async def setup(bot):
    await bot.add_cog(Image(bot))
