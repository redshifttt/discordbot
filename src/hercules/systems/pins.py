import discord
from discord.ext import commands
import aiohttp
import sqlite3
import datetime as dt
from tinydb import TinyDB, where

class PinsSystem(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.Cog.listener()
    async def on_message(self, message):
        if not message.type.value == 6:
            return

        db = TinyDB("db.json")
        row = db.search(where("server_id") == guild.id)[0]

        pins_system = row["pins_system"]
        pins_webhook_url = row["pins_webhook_url"]

        if pins_system:
            if not pins_webhook_url:
                return

            if message.reference.cached_message:
                pinned_message = message.reference.cached_message
            else:
                channel_id = message.reference.channel_id
                message_id = message.reference.message_id

                channel = await self.bot.fetch_channel(channel_id)
                pinned_message = await channel.fetch_message(message_id)

            user = pinned_message.author
            user_name = user.name
            pfp = user.avatar.url

            message_content = pinned_message.content
            message_id = pinned_message.id

            message_attachments = []
            for att in pinned_message.attachments:
                file = await att.to_file()
                message_attachments.append(file)

            async with aiohttp.ClientSession() as session:
                webhook = discord.Webhook.from_url(pins_webhook_url, session=session)
                if len(message_attachments) == 1:
                    await webhook.send(message_content, username=user_name, avatar_url=pfp, file=message_attachments[0])
                else:
                    await webhook.send(message_content, username=user_name, avatar_url=pfp, files=message_attachments)

            await pinned_message.unpin()

async def setup(bot):
    await bot.add_cog(PinsSystem(bot))
