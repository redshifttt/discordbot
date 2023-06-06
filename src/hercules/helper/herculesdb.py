import sqlite3

def connect_to_db(db):
    db_connection = sqlite3.connect(db)
    db_connection.row_factory = sqlite3.Row
    db_cursor = db_connection.cursor()

    return db_connection, db_cursor

