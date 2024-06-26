import pandas as pd

# data generated with ./dbgen -s 0.01 -vf from https://github.com/edin-dal/tpch-dbgen
PATH = "../src/test/tpch/data/SF_0.01/orders.tbl"

CUSTKEY = 751

if __name__ == "__main__":
    df = pd.read_csv(PATH, header=None, sep="|")
    df.rename(columns={1: "custkey", 8: "comment"}, inplace=True)
    df = df[df["custkey"] == CUSTKEY][["comment"]]
    df["special"] = df["comment"].str.find("special")
    df["requests"] = df["comment"].str.find("requests")
    df["sdqlpy"] = (df["special"] != -1) & (df["requests"] > df["special"] + 6)
    df["sdql"] = df["comment"].str.match(r".*special.*requests.*")
    df.to_csv("q13.csv", index=False)
