import discord
from discord.ext import commands

class Help(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.command(name="help", brief="Show this help menu", aliases=["h"])
    async def help(self, ctx, *args):
        commands = [command.name for command in list(self.bot.commands)]
        commands = sorted(commands)

        embed_content = {}
        embed_content["title"] = ":grey_question: All Commands for Hercules"
        embed_content["fields"] = []

        # embed_content["fields"].append({"name": "For more information run:", "value": "`.help <command>`", "inline": False})

        for command in commands:
            command = self.bot.get_command(command)
            command_value = ""
            command_enabled = command.enabled
            if command_enabled:
                command_name = ":white_check_mark: "
            command_name += command.name

            command_aliases = command.aliases

            command_description = command.brief
            if command_description:
                command_value += f"{command_description}\n"

            if command_aliases:
                aliases = ", ".join(command_aliases)
                command_name += f" ({aliases})"

            embed_content["fields"].append({"name": command_name, "value": command_value, "inline": False})

        embed = discord.Embed.from_dict(embed_content)

        await ctx.reply(embed=embed)
