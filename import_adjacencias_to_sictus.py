#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Prolog.

Usage:
  import [--file <data>]
  import (-h | --help)
  import --version

Options:
  -h --help      Show this screen.
  --file <data>  Data file to import [default: data/adjacencias.xlsx]
"""
from docopt import docopt
import pandas as pd

if __name__ == "__main__":
    arguments = docopt(__doc__, version="Import Script 0.1.0")
    excel = pd.read_excel(arguments["--file"], sheet_name=None)

    print("[")
    b = False
    for sheet in excel.values():
        entry = sheet.dropna()['gid'].to_list()

        for i in range(len(entry) - 1):
            sep = ", " if i != 0 else "  "
            if b:
                sep = ", "
            else:
                sep = "  "
                b = True
            print(
                sep +
                "aresta("
                + "'"
                + str(entry[i])
                + "'"
                + ","
                + "'"
                + str(entry[i + 1])
                + "'"
                + ")"
            )

    print("]")

