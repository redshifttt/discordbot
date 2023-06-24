import discord
from discord.ext import commands
import aiohttp
import sqlite3
import hercules.helper.log as log
import hercules.helper.herculesdb as db
import datetime as dt

class PinsSystem(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.Cog.listener()
    async def on_message(self, message):
        if not message.type.value == 6:
            return

        guild_id = message.guild.id

        db_connection, db_cursor = db.connect_to_db("data.db")

        row = db_cursor.execute("SELECT pins_system, general_channel FROM servers WHERE guild_id = ?", (guild_id,)).fetchone()
        pins_system = row["pins_system"]
        general_channel = row["general_channel"]

        if pins_system == 1:
            row = db_cursor.execute("SELECT pins_webhook_url, pins_blacklist FROM servers WHERE guild_id = ?", (guild_id,)).fetchone()
            pins_webhook_url = row["pins_webhook_url"]
            pins_blacklist = row["pins_blacklist"]

            if pins_blacklist is not None:
                if type(pins_blacklist) is not int:
                    channel_ids = [int(c) for c in pins_blacklist.split(",")]
                    if channel.id in channel_ids:
                        return
                else:
                    if channel.id == pins_blacklist:
                        return

            if pins_webhook_url is None:
                await general_channel.send(":x: No webhook URL set for the Pins System")
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
    log.in_log("INFO", "listener_setup", "Pins System has been loaded")
    await bot.add_cog(PinsSystem(bot))
