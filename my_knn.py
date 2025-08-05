import numpy as np
from scipy.spatial.distance import cdist
from sklearn.preprocessing import StandardScaler

def my_knn_py(train, test, train_labels, k=5, distance="euclidean", 
           scaling=True, weighted=False, dmatrix=None):
    
    train = np.array(train)
    test = np.array(test)
    train_labels = np.array(train_labels)
    
    if dmatrix is None:
        D = np.vstack((train, test))
        if scaling:
            D = StandardScaler().fit_transform(D)
        D1 = cdist(D, D, metric=distance)
    else:
        D1 = np.array(dmatrix)
    
    N_train = train.shape[0]
    N_test = test.shape[0]
    
    K_indices = np.full((N_test, k), np.nan)
    K_labels = np.full((N_test, k), np.nan)
    PRED = np.full(N_test, np.nan)

    for i in range(N_test):
        test_idx = N_train + i
        dists = D1[test_idx, :N_train]
        sorted_idx = np.argsort(dists)[:k]
        lbls = train_labels[sorted_idx]

        K_indices[i, :] = sorted_idx
        K_labels[i, :] = lbls

        if not weighted:
            # Majority vote
            values, counts = np.unique(lbls, return_counts=True)
            PRED[i] = values[np.argmax(counts)]
        else:
            # Weighted vote: 1 / distance
            selected_dists = dists[sorted_idx]
            selected_dists[selected_dists == 0] = 1e-6  # avoid divide-by-zero
            weights = 1 / selected_dists
            wtbl = {}
            for lbl, w in zip(lbls, weights):
                wtbl[lbl] = wtbl.get(lbl, 0) + w
            PRED[i] = max(wtbl, key=wtbl.get)
    
    return {
        "prediction": PRED,
        "neighbors": K_indices,
        "labels": K_labels
    }
