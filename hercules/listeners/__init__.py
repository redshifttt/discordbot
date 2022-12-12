import discord
from discord.ext import commands
import aiohttp
import datetime as dt
import sqlite3

class BotListeners(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.Cog.listener(name='on_guild_channel_pins_update')
    async def pins(self, channel, last_pin):
        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()

        if not last_pin:
            return

        channel_pins = await channel.pins()
        if not channel_pins:
            return

        for pin in channel_pins:
            name = pin.author.name
            pfp = pin.author.avatar.url
            content = pin.content

            attachments = None
            if pin.attachments:
                attachments = [att.url for att in pin.attachments]
                content = f"{content} {' '.join(attachments)}"

            async with aiohttp.ClientSession() as session:
                pinhook = discord.Webhook.from_url(conf.pinhook, session=session) # TODO: modularise
                await pinhook.send(content=content, username=name, avatar_url=pfp)

            await pin.unpin()

    @commands.Cog.listener(name='on_member_join')
    async def member_join(self, member):
        traffic = bot.get_channel() # TODO: modularise
        verification = bot.get_channel(conf.loft_verification_channel) # TODO: modularise

        verification_message = f"hey {member.mention}, say something then ping someone who's online to get in. you'd be surprised how hard this is for some people."
        await traffic.send(f":inbox_tray: **{member}** has joined the server!")
        await verification.send(verification_message)
        await update_status_message()

    @commands.Cog.listener(name='on_member_remove')
    async def member_leave(self, member):
        traffic = bot.get_channel(conf.loft_traffic_channel) # TODO: modularise

        await traffic.send(f":outbox_tray: **{member}** has left the server.")
        await update_status_message()

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

    @commands.Cog.listener(name='on_message')
    async def invite_nuker(self, message):
        if "discord.gg" in message.content:
            await message.delete()
            await message.author.timeout(dt.timedelta(minutes=1), reason=f"Advertising in #{message.channel.name}")

    @commands.Cog.listener(name='on_guild_join')
    async def db_server_init(self, guild):
        guild_id = guild.id

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()

        db_cur.execute("INSERT INTO servers VALUES(?, ?, ?, ?, ?)", (guild_id, null, null, null, null))
        db_con.commit()

