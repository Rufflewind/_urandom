#!/usr/bin/env python
import harvester, sqlite3, utils, sys
harvester.CHUNK_SIZE = int(sys.argv[1])
with sqlite3.connect("test.sqlite3", timeout=120, isolation_level=None) as db:
    db.cursor().execute("""
    CREATE TABLE IF NOT EXISTS data (rowid INTEGER PRIMARY KEY)
    """)
    for i in range(1):
        with open("input.log") as f:
            out = f.read()
        data = utils.interpret_output(out)
        with utils.Transaction(db, behavior="IMMEDIATE"):
            utils.sql_insert_many(db, "data", data)
