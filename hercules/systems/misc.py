import discord
from discord.ext import commands
import aiohttp
import datetime as dt
import sqlite3
import hercules.helper.log as log

class MiscListeners(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.Cog.listener(name='on_message')
    async def message_link_resolve(self, message):
        if "discord.com/channels" not in message.content:
            return

        split_content = message.content.split()

        for val in split_content:
            if "/channels" in val:
                link = val

        *_, channel_id, message_id = link.split("/")

        found_channel = message.guild.get_channel(int(channel_id))
        found_message = await found_channel.fetch_message(int(message_id))

        user_name = found_message.author.name
        content = found_message.clean_content

        if found_message.embeds:
            content = "**`Cannot have an embed inside and embed.`**"

        embed = discord.Embed(title=f"{user_name}'s message", description=content)

        embed.set_author(name=message.guild.name, icon_url=message.guild.icon.url)
        embed.set_footer(text=f"Linked by {message.author.name}", icon_url=message.author.avatar.url)
        embed.add_field(name="Jump to message", value=f"[here]({link})")

        await message.reply(embed=embed)

    @commands.Cog.listener(name='on_guild_join')
    async def db_server_init(self, guild):
        guild_id = guild.id

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()

        db_cur.execute("INSERT INTO servers VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?)", (guild_id, null, null, null, null, null, null, null, null,))
        db_con.commit()

async def setup(bot):
    log.in_log("INFO", "listener_setup", "listener MiscListeners has been loaded")
    await bot.add_cog(MiscListeners(bot))