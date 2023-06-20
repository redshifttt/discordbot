import discord
from discord.ext import commands
import hercules.helper.log as log
import hercules.helper.utils as herculesutils
import time
import datetime

class Snoop(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    # async def get_channel_messages(self, channel_id):

    @commands.group()
    async def snoop(self, ctx):
        if ctx.invoked_subcommand:
            return
        await ctx.reply(";)")

    @snoop.group()
    async def guild(self, ctx, guild_arg):
        guild = self.bot.get_guild(int(guild_arg))
        categories = guild.categories
        channels_list = ""

        channels_list = f"**__Channels list for {guild.name}__**\n"
        for cat in categories:
            channels_list += f"**→ {cat.name}**\n"
            for channel in cat.channels:
                channels_list += "\t"
                channels_list += f"{channel.name} `{channel.id}`"
                channels_list += "\n"

        await ctx.reply(channels_list)

    @snoop.group()
    async def channel(self, ctx, channel_arg):
        channel = self.bot.get_channel(int(channel_arg))

        log = ""
        messages = []

        log += f"Last 500 messages in {str(channel.guild)} -> #{str(channel)}\n"
        log += "Times are in UTC.\n\n"

        last_message_creation = datetime.datetime(1970, 1, 1)

        async for message in channel.history(limit=500):
            messages.append(message)

        messages = reversed(messages)

        for message in messages:
            author = message.author

            content = message.clean_content

            if not content:
                content = message

            created_at = message.created_at
            created_at_fancy = created_at.strftime("%H:%M:%S")

            if last_message_creation.date() < created_at.date():
                log += f"--- {created_at.strftime('%d %B %Y')} ---\n"

            log += f"[{created_at_fancy}]"

            if author.bot:
                log += " 🤖 "
            else:
                log += " "

            log += f"{author.name}: {content}\n"

            last_message_creation = created_at

            with open(f"Hercules snoop for {channel.name}.txt", "w") as f:
                f.write(log)

        await ctx.reply(file=discord.File(f"Hercules snoop for {channel.name}.txt"))

    @snoop.group()
    async def activity(self, ctx, channel_arg):
        channel = self.bot.get_channel(int(channel_arg))

        log = ""
        messages = []
        dates = {}

        log += f"Last week of activity in {str(channel.guild)} -> #{str(channel)}\n"

        today = datetime.datetime.today()
        week_ago_td = datetime.timedelta(days=7)
        date_week_ago = today - week_ago_td
        print(date_week_ago)

        async with self.bot.get_channel(ctx.channel.id).typing():
            async for message in channel.history(after=date_week_ago, oldest_first=True, limit=None):
                messages.append(message)

            log += f"Total: {len(messages)} posts\n"

            for message in messages:
                print(message)
                posted_on = message.created_at.strftime("%a %d %B %Y")
                if posted_on not in dates:
                    dates[posted_on] = 1
                else:
                    dates[posted_on] += 1

            for k, v in dates.items():
                log += f"{k}: {v} posts\n"

            ts = time.time()
            with open(f"Hercules activity for {channel.name} {round(ts)}.txt", "w") as f:
                f.write(log)
            await ctx.reply(file=discord.File(f"Hercules activity for {channel.name} {round(ts)}.txt"))

async def setup(bot):
    log.in_log("INFO", "command_setup", "command snoop has been loaded")
    await bot.add_cog(Snoop(bot))
