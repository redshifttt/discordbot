import discord
from discord.ext import commands
import requests
import json
import hercules.helper.log as log

class Search(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.command(
        name="search",
        brief="Search the internet with search.privatevoid.net",
        help="Search the internet with search.privatevoid.net\n\n**Usage**\n`./search your search terms`",
        aliases=["s"]
    )
    async def search(self, ctx, *args):
        async with self.bot.get_channel(ctx.channel.id).typing():
            # Turn command arguments (tuple) into a list
            search_term = list(args)
            url = f"https://search.privatevoid.net/search?q={search_term}&format=json"
            url_search_term = f"https://search.privatevoid.net/search?q={'+'.join(search_term)}"

            results = json.loads(
                requests.get(f"https://search.privatevoid.net/search?q={search_term}&format=json").text
            )["results"][:5]

            embed = discord.Embed(title=f"First 5 search results for \"{' '.join(search_term)}\"", url=url_search_term)

            for _, r in enumerate(results, 1):
                site_title = f"┃ {r['title']}"
                site_url = r["url"]
                site_description = f"{site_url}\n{r['content'][:150]}..."

                embed.add_field(name=site_title, value=site_description, inline=False)

            embed.set_author(name=f"{ctx.author.name}'s query", icon_url=ctx.author.avatar.url)
            embed.set_footer(
                text="All search queries are private | search.privatevoid.net",
                icon_url="https://cdn.privatevoid.net/private-void/branding/2022/hexagon-small-white-outline.png"
            )
            await ctx.reply(embed=embed)

async def setup(bot):
    log.in_log("INFO", "command_setup", "command search has been loaded")
    await bot.add_cog(Search(bot))
