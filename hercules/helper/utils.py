async def handle_mention_or_id(bot, ctx, user_identifier):
    if not user_identifier:
        return ctx.author

    user_mentions = ctx.message.mentions
    if user_mentions:
        return user_mentions[0]

    if user_identifier.isnumeric():
        return ctx.guild.get_member(int(user_identifier)) or await bot.fetch_user(int(user_identifier))
