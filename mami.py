import numpy as np
from scipy.spatial.distance import cdist
from sklearn.preprocessing import StandardScaler

def mami_py(train, test, train_labels, k1=5, k2=10, 
         distance='euclidean', scaling=True, dmatrix=None):
    
    n_train = train.shape[0]
    n_test = test.shape[0]
    all_data = np.vstack([train, test])

    # Compute distance matrix if not provided
    if dmatrix is None:
        if scaling:
            all_data = StandardScaler().fit_transform(all_data)
        D1 = cdist(all_data, all_data, metric=distance)
    else:
        D1 = dmatrix

    # First-layer neighbors
    K1 = np.full((n_test, k1), np.nan)
    K1_l = np.full((n_test, k1), np.nan)

    for i in range(n_test):
        test_idx = n_train + i
        dists = D1[test_idx, :n_train]
        nearest = np.argsort(dists)[:k1]
        K1[i, :] = nearest
        K1_l[i, :] = train_labels[nearest]

    # Second-layer neighbors
    K2_list = []

    for i in range(n_test):
        K2 = np.full((k1, k2), np.nan)
        for j in range(k1):
            idx = int(K1[i, j])
            dists = D1[idx, :n_train]
            nearest = np.argsort(dists)[:k2]
            K2[j, :] = train_labels[nearest]
        K2_list.append(K2)

    # Coverage calculation
    C_list = []
    for i in range(n_test):
        C = np.zeros(k1)
        for j in range(k1):
            C[j] = np.sum(K2_list[i][j, :] == K1_l[i, j])
        C_list.append(C)

    # Predict and compute coverage matrix
    PRED = []
    COV_ALL = []

    for i in range(n_test):
        labels, counts = np.unique(K1_l[i, :], return_counts=True)
        COV = {}
        for lbl in labels:
            mask = K1_l[i, :] == lbl
            COV[str(int(lbl))] = np.sum(C_list[i][mask]) / (k1 * k2)
        COV_ALL.append(COV)
        pred_label = int(max(COV, key=COV.get))
        PRED.append(pred_label)

    # Build final coverage matrix
    unique_labels = np.unique(train_labels)
    label_to_idx = {str(int(lbl)): i for i, lbl in enumerate(unique_labels)}
    COV = np.zeros((n_test, len(unique_labels)))

    for i, cov_dict in enumerate(COV_ALL):
        for lbl_str, val in cov_dict.items():
            j = label_to_idx[lbl_str]
            COV[i, j] = val

    return {
        'prediction': np.array(PRED),
        'coverage': COV
    }