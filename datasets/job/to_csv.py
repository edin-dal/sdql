import csv
import os

import pandas as pd

if __name__ == '__main__':
	except_queries = []
	for path, dir_list, file_list in os.walk(os.path.dirname(__file__)):
		for file_name in file_list:
			if file_name.endswith(".parquet"):
				file_path = os.path.join(path, file_name)
				print(file_path)
				df = pd.read_parquet(file_path)
				for col in df.columns:
					if df[col].dtype == "int32":
						df[col] = df[col].fillna(-1)
					elif df[col].dtype == "float64":
						df[col] = df[col].fillna(-1).astype("int32")
					elif df[col].dtype == "object":
						df[col] = df[col].str.replace('"', '').replace('|', '')

				file_path = file_path.replace(".parquet", ".csv")
				try:
					df.to_csv(
						file_path,
						index=False,
						header=False,
						sep="|",
						quoting=csv.QUOTE_NONE
					)
				except:
					except_queries.append(file_path)
					df.to_csv(
						file_path,
						index=False,
						header=False,
						sep="|",
						quoting=csv.QUOTE_MINIMAL
					)

	print("Except queries:", except_queries)
