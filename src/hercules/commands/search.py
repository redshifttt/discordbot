import discord
from discord.ext import commands
import requests
import json

class Search(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.command(name="search", aliases=["s"])
    async def search(self, ctx, *args):
        async with self.bot.get_channel(ctx.channel.id).typing():
            # Turn command arguments (tuple) into a list
            search_term = list(args)
            url = f"https://search.privatevoid.net/search?q={search_term}&format=json"
            url_search_term = f"https://search.privatevoid.net/search?q={'+'.join(search_term)}"

            results = json.loads(requests.get(url).text)["results"][:5]

            embed = discord.Embed(title=f"First 5 search results for \"{' '.join(search_term)}\"")
            embed.description = f"[Full search results]({url_search_term})"

            for _, r in enumerate(results, 1):
                site_title = f"{r['title']}"
                site_url = r["url"]
                site_description = f":link: {site_url}\n{r['content'][:150]}..."

                embed.add_field(name=site_title, value=site_description, inline=False)

            embed.set_author(name=f"{ctx.author.name}'s query", icon_url=ctx.author.avatar.url)
            embed.set_footer(
                text="All search queries are private | search.privatevoid.net",
                icon_url="https://cdn.privatevoid.net/private-void/branding/2022/hexagon-small-white-outline.png"
            )
            await ctx.reply(embed=embed)

async def setup(bot):
    await bot.add_cog(Search(bot))
