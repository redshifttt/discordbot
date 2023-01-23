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
            embed_content["title"] = ":grey_question: All Commands for Hercules"
            embed_content["fields"] = []

            embed_content["fields"].append({"name": "For more information run:", "value": "`.help <command>`", "inline": False})

            for command in commands:
                command = self.bot.get_command(command)
                command_value = ""
                command_enabled = command.enabled
                if command_enabled:
                    command_name = ":white_check_mark: "
                else:
                    command_name = ":x: "
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

        # This only runs when given an argument
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
        embed_content["description"] = command_description

        command_aliases = command.aliases
        if command_aliases:
            aliases = ", ".join(command_aliases)
            embed_content["fields"].append({"name": "Aliases", "value": aliases, "inline": False})

        try:
            if command.commands:
                subcommands = command.commands
                all_subcommands = ""
                for sub in list(command.commands):
                    name = sub.name
                    description = f"{sub.help}\n" or ""
                    if sub.aliases:
                        aliases = ", ".join(sub.aliases)
                        description += f"\nAliases: {aliases}\n"
                    all_subcommands += f"`{name}`: {description}\n"
                embed_content["fields"].append({"name": "Subcommands", "value": all_subcommands, "inline": False})
        except:
            pass

        embed = discord.Embed.from_dict(embed_content)

        await ctx.reply(embed=embed)

async def setup(bot):
    log.in_log("INFO", "command_setup", "command help has been loaded")
    await bot.add_cog(Help(bot))
