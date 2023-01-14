import discord
from discord.ext import commands

class Help(commands.Cog):
    def __init__(self, bot):
        self.bot = bot

    @commands.command(name="help", brief="Show this help menu", aliases=["h"])
    async def help(self, ctx, *args):
        if not args:
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
            return
        else:
            command = self.bot.get_command(args[0])
            if not command:
                await ctx.reply("Command not found")
                return

            embed_content = {}
            embed_content["title"] = f"{command.name} command"
            embed_content["fields"] = []

            command_enabled = command.enabled
            if command_enabled:
                embed_content["title"] = f":white_check_mark: {embed_content['title']}"

            command_description = command.help
            if command_description:
                embed_content["description"] = command_description
            else:
                embed_content["description"] = command.brief

            command_aliases = command.aliases
            if command_aliases:
                aliases = ", ".join(command_aliases)
                embed_content["fields"].append({"name": "Aliases", "value": aliases, "inline": False})

            try:
                if command.commands:
                    subcommands = ""
                    for sub in list(command.commands):
                        name = sub.name
                        description = sub.help or sub.brief or ""
                        subcommands += f"`{name}`: {description}\n"
                    embed_content["fields"].append({"name": "Subcommands", "value": subcommands, "inline": False})
            except:
                pass

            embed = discord.Embed.from_dict(embed_content)

            await ctx.reply(embed=embed)
