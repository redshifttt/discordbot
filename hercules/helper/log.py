import discord
import datetime as dt

def in_log(log_type: str, method_name: str, message: str):
    utc_now = dt.datetime.utcnow().strftime("%F %T.%f UTC")
    log_message = f"[{utc_now}] [{log_type}] [{method_name}]: {message}"
    print(log_message)
