import json
import pickle
import sys

from functools import partial


import numpy as np
import pandas as pd


from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import RBF
from sklearn.preprocessing import StandardScaler


def fit_gp(df: pd.DataFrame, i_vars: list, d_var: str) -> dict:
    """z-standardize x vars and predict y using gp with rbf kernel

    Args:
        df (pd.DataFrame): training data
        i_vars (list): x vars
        d_var (str): y var

    Returns:
        dict: fitted parameter values of scaler and gp model
    """
    m_scale = StandardScaler()
    m_scale.fit_transform(df[i_vars])
    k_rbf = RBF()
    m_gp = GaussianProcessRegressor(kernel=k_rbf)
    m_gp.fit(df[i_vars], df[d_var])
    dict_results = {
        "length_scale": m_gp.kernel_.length_scale,
        "x_mean": m_scale.mean_,
        "x_sd": m_scale.scale_,
    }
    return dict_results


sys.path.append("../../")

with open("data/l-data-train.json", "rb") as f:
    dict_data_train = json.load(f)

l_dfs_train = list()
l_dfs_train = [pd.DataFrame(dict_data_train[k]) for k in dict_data_train.keys()]

df = l_dfs_train[1]
i_vars = ["x1", "x2"]
d_var = ["y"]
fit_gp_partial = partial(fit_gp, i_vars=i_vars, d_var=d_var)
l_models = list(map(fit_gp_partial, l_dfs_train))

folder_location = "data/"
file_name = "model-params.pickle"

with open(folder_location + file_name, "wb") as f:
    pickle.dump(l_models, f)
