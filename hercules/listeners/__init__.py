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
        guild_id = channel.guild.id

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()

        has_pins_function = db_cur.execute("SELECT pins_channel FROM servers WHERE guild_id=?", (guild_id,)).fetchone()

        if has_pins_function[0] == "null":
            return

        pins_channel = db_cur.execute("SELECT pins_channel FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]
        pins_channel = self.bot.get_channel(int(pins_channel))

        available_webhooks = await pins_channel.webhooks()
        hercules_pinhook = None

        for hook in available_webhooks:
            if "Hercules Pin Webhook" in hook.name:
                webhook = hook.url
            else:
                webhook = await pins_channel.create_webhook(name="Hercules Pin Webhook").url

        if not last_pin:
            return

        channel_pins = await channel.pins()
        if not channel_pins:
            channel.send(":x: No pins in this channel.")
            return

        pin = channel_pins[0]

        name = pin.author.name
        pfp = pin.author.avatar.url
        content = pin.content

        attachments = None
        if pin.attachments:
            attachments = [att.url for att in pin.attachments]
            content = f"{content} {' '.join(attachments)}"

        # TODO: just do the await webhook.send
        async with aiohttp.ClientSession() as session:
            pinhook = discord.Webhook.from_url(webhook, session=session) # TODO: modularise
            await pinhook.send(content=content, username=name, avatar_url=pfp)

        await pin.unpin()

    @commands.Cog.listener(name='on_member_join')
    async def member_join(self, member):
        guild_id = member.guild.id

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()

        traffic_channel = db_cur.execute("SELECT traffic_channel FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]
        verification_channel = db_cur.execute("SELECT verification_channel FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]

        if not traffic_channel == "null":
            traffic_channel = self.bot.get_channel(int(traffic_channel))
        else:
            # TODO: maybe have a setting where you can set the general channel
            # then have a message be sent there as an error in the settings.
            # Goes for verification_channel as well.
            return

        if not verification_channel == "null":
            verification_channel = self.bot.get_channel(int(verification_channel))
        else:
            return

        # TODO: Make a setting for the message in the future. An idea is to let
        # the user specify something like {user_member} when setting the value for
        # verification_message; you can check if the string has that in it
        # and just replace it with member.mention or something. The same goes
        # for the message sent to traffic_channel,
        verification_message = f"hey {member.mention}, say something then ping someone who's online to get in. you'd be surprised how hard this is for some people."
        await traffic_channel.send(f":inbox_tray: **{member}** has joined the server!")

        await verification_channel.send(verification_message)

    @commands.Cog.listener(name='on_member_remove')
    async def member_leave(self, member):
        guild_id = member.guild.id

        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()

        traffic_channel = db_cur.execute("SELECT traffic_channel FROM servers WHERE guild_id=?", (guild_id,)).fetchone()[0]
        if not traffic_channel == "null":
            traffic_channel = self.bot.get_channel(int(traffic_channel))
        else:
            # Same idea as above with the general channel.
            return

        await traffic_channel.send(f":outbox_tray: **{member}** has left the server.")
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

        db_cur.execute("INSERT INTO servers VALUES(?, ?, ?, ?, ?)", (guild_id, null, null, null, null,))
        db_con.commit()

