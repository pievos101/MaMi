# Call make_classication from Python 

# Load the reticulate package
library(reticulate)

# Use a specific Python environment (optional)
# use_virtualenv("myenv") or use_python("/usr/bin/python3")

# Import Python modules
sklearn <- import("sklearn.datasets")

n_iter = 30

RES = matrix(NaN, n_iter, 6)
colnames(RES) = c("MaMi_k1","MaMi_k10","MaMi_k50","MaMi_k100", "kNN","wKNN")

source("~/GitHub/MaMi/REVISION2/my_knn.R")

for(xx in 1:n_iter){

    # Create dataset with proper integer parameters
    dataset <- sklearn$make_classification(
    n_samples = as.integer(2500),
    n_features = as.integer(20),
    n_informative = as.integer(5),
    n_redundant = as.integer(2),
    n_classes = as.integer(2),
    weights = list(0.5, 0.5),
    flip_y = 0,
    random_state = as.integer(42)
    )

    # Convert output to R
    X <- dataset[[1]]
    y <- dataset[[2]] + 1 # HACK!

    # Turn into data frame
    data <- as.matrix(X)
    target <- as.numeric(y)

    NN = paste("V", 1:ncol(data), sep="")
    colnames(data) = NN
    names(target) = NN

    # Train-test split 

    ## 80% of the sample size
    smp_size <- floor(0.80 * nrow(data))

    train_ind <- sample(seq_len(nrow(data)), size = smp_size)

    train <- data[train_ind, ]
    test  <- data[-train_ind, ]

    train_labels = target[train_ind]
    test_labels  = target[-train_ind]

    library(caret)
    library(ModelMetrics)
    library(MLmetrics)

    probs = TRUE

    # Call the Major-Minority algorithm
    source("~/GitHub/MaMi/mami.r")
    MAMI_k1_time = system.time({
    res = mami(train, test, train_labels, k1=3, k2=1)
    })
    MAMI_k3_time = system.time({
    res = mami(train, test, train_labels, k1=3, k2=10)
    })
    MAMI_k5_time = system.time({
    res = mami(train, test, train_labels, k1=3, k2=50)
    })
    MAMI_k10_time = system.time({
    res = mami(train, test, train_labels, k1=3, k2=100)
    })
    
    
    KNN_time = system.time({
        my_knn(train, test, train_labels, k = 3, weighted = FALSE)
    })
    KNN_time_w = system.time({
        my_knn(train, test, train_labels, k = 3, weighted = TRUE)
    })
    
    

RES[xx,1] = MAMI_k1_time[3] #MAMI_perf
RES[xx,2] = MAMI_k3_time[3] #MAMI_perf
RES[xx,3] = MAMI_k5_time[3] #MAMI_perf
RES[xx,4] = MAMI_k10_time[3] #MAMI_perf
RES[xx,5] = KNN_time[3]
RES[xx,6] = KNN_time_w[3]

print(RES)

}