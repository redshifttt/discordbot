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
    async def on_guild_channel_pins_update(self, channel, lastpin):
        guild_id = channel.guild.id

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

            pinned_messages = [m async for m in channel.history(limit=100) if m.pinned]

            for message in pinned_messages:
                user = message.author
                user_name = f"{user.name}#{user.discriminator}"
                pfp = user.avatar.url
                message_content = message.content + "\n"
                message_id = message.id
                message_attachments = [att.url for att in message.attachments]
                message_content += "\n".join(message_attachments)

                pin_in_db = db_cursor.execute("SELECT pinned_message_id FROM pins WHERE guild_id = ?", (guild_id,)).fetchall()
                found_pins = [pin["pinned_message_id"] for pin in pin_in_db]

                if message_id in found_pins:
                    await message.unpin()
                    return

                async with aiohttp.ClientSession() as session:
                    webhook = discord.Webhook.from_url(pins_webhook_url, session=session)
                    await webhook.send(message_content, username=user_name, avatar_url=pfp)

                db_cursor.execute("INSERT INTO pins (guild_id, pinned_message_id) VALUES (?, ?)", (guild_id, message_id,))

                db_connection.commit()

                await message.unpin()

                # we only want the last pinned message to be put in pins_channel
                return

        db_connection.close()

async def setup(bot):
    log.in_log("INFO", "listener_setup", "Pins System has been loaded")
    await bot.add_cog(PinsSystem(bot))
