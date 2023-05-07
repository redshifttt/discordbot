module Hercules.CommandParameters.Types where
import Discord.Interactions

string :: OptionValue
string = OptionValueString {
  optionValueName = "string",
  optionValueDescription = "A string",
  optionValueLocalizedName = Nothing,
  optionValueLocalizedDescription = Nothing,
  optionValueStringMinLen = Nothing,
  optionValueStringMaxLen = Nothing,
  optionValueStringChoices = Left False,
  optionValueRequired = True
}

subcommand :: OptionSubcommand
subcommand = OptionSubcommand {
  optionSubcommandName = "subcommand",
  optionSubcommandDescription = "A subcommand",
  optionSubcommandLocalizedName = Nothing,
  optionSubcommandLocalizedDescription = Nothing,
  optionSubcommandOptions = []
}
