"""Prolog.

Usage:
  import [--file <data>]
  import (-h | --help)
  import --version

Options:
  -h --help      Show this screen.
  --file <data>  Data file to import [default: data/paragens_preprocessado.csv]
"""
import pandas as pd

if __name__ == "__main__":
    carreiras = pd.read_csv("carreiras.csv").dropna()

    for index, row in carreiras.iterrows():
        print(
            "carreira("
            + "'"
            + str(row["gid"])
            + "'"
            + ","
            + str(row["latitude"])
            + ","
            + str(row["longitude"])
            + ","
            + "'"
            + row["Estado de Conservacao"]
            + "'"
            + ","
            + "'"
            + row["Tipo de Abrigo"]
            + "'"
            + ","
            + "'"
            + row["Abrigo com Publicidade?"]
            + "'"
            + ","
            + "'"
            + row["Operadora"]
            + "'"
            + ","
            + "'"
            + str(row["Carreira"])
            + "'"
            + ","
            + "'"
            + str(row["Codigo de Rua"])
            + "'"
            + ","
            + "'"
            + row["Nome da Rua"]
            + "'"
            + ","
            + "'"
            + row["Freguesia"]
            + "'"
            + ")."
        )
