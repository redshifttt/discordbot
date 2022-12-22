import discord
from discord.ext import commands
import sqlite3

class Settings(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.command(name="settings")
    async def settings(self, ctx, *args):
        db_con = sqlite3.connect("data.db")
        db_cur = db_con.cursor()
        guild_id = ctx.guild.id

        if not args:
            guild_id, traffic_channel, pins_channel, verification_channel, general_channel, logs_channel, join_message, leave_message, verification_message \
                = db_cur.execute("SELECT * FROM servers WHERE guild_id=?", (guild_id,)).fetchall()[0]

            embed_content = {
                "title": f"Hercules settings for {ctx.guild.name}",
                "fields": [
                    { "name": "Traffic Channel", "value": traffic_channel, "inline": False },
                    { "name": "Pins Channel", "value": pins_channel, "inline": False },
                    { "name": "Verification Channel", "value": verification_channel, "inline": False },
                    { "name": "General Channel", "value": general_channel, "inline": False },
                    { "name": "Logs Channel", "value": logs_channel, "inline": False },
                    { "name": "Join Message", "value": join_message, "inline": False },
                    { "name": "Leave Message", "value": leave_message, "inline": False },
                    { "name": "Verification Message", "value": verification_message, "inline": False }
                ]
            }

            embed = discord.Embed().from_dict(embed_content)
            embed.set_thumbnail(url=ctx.guild.icon.url)

            await ctx.reply(embed=embed)
            return

        if not len(args) == 2:
            await ctx.reply(":x: wrong number of arguments")
            return

        header, value = args

        if header == "guild_id":
            await ctx.reply(f":x: Cannot change the current server ID")
            return

        res = db_cur.execute("SELECT guild_id FROM servers WHERE guild_id=?", (guild_id,))

        # Unfortunately the below doesn't work. Time to open myself up to SQL
        # injections - not like it matters though.
        # res = db_cur.execute("UPDATE servers SET ? = ? WHERE guild_id=?", (header, value, guild_id,))

        # This is what seems to be the best you can do when it comes to dynamic settings of values
        res = db_cur.execute(f"UPDATE servers SET {header} = '{value}' WHERE guild_id={guild_id}")

        if not res:
            await ctx.reply(f":x: There was an error setting that value.")
            return

        await ctx.reply(f":white_check_mark: {header} set to `{value}`")
        db_con.commit()
        db_con.close()
