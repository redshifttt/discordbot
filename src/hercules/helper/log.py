import datetime as dt
import inspect

BOLD = "\x1b[1m"
COLOUR_OFF = "\x1b[0m"

RED = "\x1b[31m"
RED_BOLD = "\x1b[1;31m"
YELLOW_BOLD = "\x1b[1;33m"
BLUE = "\x1b[34m"

def in_log(log_type: str, method_name: str, message: str):
    utc_now = dt.datetime.utcnow().strftime("%F %T.%f UTC")

    stack_trace_to_call = inspect.stack()[1]
    file_with_log_call = "/".join(stack_trace_to_call.filename.split("/")[-3:])
    method_with_log_call = stack_trace_to_call.function
    line_with_log_call = stack_trace_to_call.lineno

    match log_type:
        case "ERROR":
            log_type = f"{RED_BOLD}{log_type}{COLOUR_OFF}"
        case "INFO":
            log_type = f"{YELLOW_BOLD}{log_type}{COLOUR_OFF}"

    log_message = f"[{BOLD}{utc_now}{COLOUR_OFF}] [{log_type}] [{BLUE}{file_with_log_call}{COLOUR_OFF} @ {RED}{method_with_log_call}(){COLOUR_OFF} line {BLUE}{line_with_log_call}{COLOUR_OFF}]: {message}"

    with open("bot.log", "a") as f:
        f.write(log_message + "\n")

    print(log_message)
