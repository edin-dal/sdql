import os

import numpy as np
import pandas as pd

from benchmarks.helpers import RTOL


def validate_results(true_results_path, exp_results_path):
    def are_dfs_equal(df1: pd.DataFrame, df2: pd.DataFrame) -> bool:
        np1, np2 = df1.to_numpy(), df2.to_numpy()
        if np1.shape != np2.shape:
            return False

        flag = True
        for idx in range(np1.shape[1]):
            col1, col2 = np1[:, idx], np2[:, idx]

            if isinstance(col1[0], float) or isinstance(col2[0], float):
                flag &= (
                    np.isclose(
                        np.array(col1, dtype=float),
                        np.array(col2, dtype=float),
                        rtol=RTOL,
                    )
                ).all()
            elif isinstance(col1[0], str):
                col1, col2 = np.char.strip(np.array(col1, dtype=str)), np.char.strip(
                    np.array(col2, dtype=str)
                )
                if (np.char.str_len(col1) > 35).any() or (
                    np.char.str_len(col2) > 35
                ).any:
                    continue
                flag &= (col1 == col2).all()
            else:
                flag &= (col1 == col2).all()
        return flag

    invalid_queries = list()
    unknown_queries = list()

    true_results = {
        filename[:-4]: pd.read_csv(
            os.path.join(true_results_path, filename), header=None, index_col=None
        )
        for filename in os.listdir(true_results_path)
    }

    for filename in sorted(os.listdir(exp_results_path)):
        query_name = filename[:-4]
        try:
            exp_res = pd.read_csv(
                os.path.join(exp_results_path, filename), header=None, index_col=None
            )
            if query_name not in true_results.keys():
                unknown_queries.append(query_name)
            elif not are_dfs_equal(true_results[query_name], exp_res):
                invalid_queries.append(query_name)
        except:
            invalid_queries.append(query_name)

    return invalid_queries, unknown_queries
