import discord
from discord.ext import commands
import hercules.helper.log as log

class Help(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.command(name="help", brief="Show this help menu", aliases=["h"])
    async def help(self, ctx, *args):
        if not args:
            commands = [command.name for command in list(self.bot.commands)]
            commands = sorted(commands)

            embed_content = {}
            embed_content["title"] = ":grey_question: All Enabled Commands for Hercules"
            embed_content["fields"] = []

            for command in commands:
                command = self.bot.get_command(command)

                command_enabled = command.enabled
                if not command_enabled:
                    continue

                command_value = ""
                command_name = f"`./{command.name}`"

                command_description = command.brief
                if command_description:
                    command_value += f"**Description:** {command_description}\n"

                command_aliases = command.aliases
                if command_aliases:
                    aliases = ", ".join(command_aliases)
                    command_value += f"**Aliases:** {aliases}\n"

                try:
                    if command.commands:
                        command_value += "**Subcommands:**\n"
                        for sub in list(command.commands):
                            command_value += f"- `./{command.name} {sub.name}`"

                            if sub.aliases:
                                aliases = ", ".join(sub.aliases)
                                command_value += f" `({aliases})`\n"

                            if sub.description:
                                command_value += f"{sub.description}\n"
                except:
                    pass

                embed_content["fields"].append({"name": command_name, "value": command_value, "inline": False})

            embed = discord.Embed.from_dict(embed_content)

            await ctx.reply(embed=embed)

async def setup(bot):
    log.in_log("INFO", "command_setup", "command help has been loaded")
    await bot.add_cog(Help(bot))
