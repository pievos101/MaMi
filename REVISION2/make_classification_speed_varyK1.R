# Call make_classication from Python 

# Load the reticulate package
library(reticulate)

# Use a specific Python environment (optional)
# use_virtualenv("myenv") or use_python("/usr/bin/python3")

# Import Python modules
sklearn <- import("sklearn.datasets")

n_iter = 30

RES = matrix(NaN, n_iter, 3)
colnames(RES) = c("MaMi","kNN","wKNN")

source("~/GitHub/MaMi/REVISION2/my_knn.R")

for(xx in 1:n_iter){

    # Create dataset with proper integer parameters
    dataset <- sklearn$make_classification(
    n_samples = as.integer(5000),
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

    # Calculate Distance Matrix
    scaling = TRUE
    distance = "euclidean"
    D  = rbind(train, test)

    if(scaling){
        D1 = as.matrix(dist(scale(D), method=distance))
    }else{
        D1 = as.matrix(dist(D))
    }

    probs = TRUE

    # Call the Major-Minority algorithm
    source("~/GitHub/MaMi/mami.r")
    source("~/GitHub/MaMi/mami_fast.r")
    source_python("/home/bastian/GitHub/MaMi/mami.py")
    source_python("/home/bastian/GitHub/MaMi/my_knn.py")
    

    train_labels_py = np_array(train_labels)

    MAMI_time = system.time({
    res = mami_py(train, test, train_labels_py, k2=as.integer(3), 
                        k1=as.integer(10), dmatrix=D1)
    })
    
    
    # standard kNN
    KNN_time = system.time({
        my_knn_py(train, test, train_labels_py, k = as.integer(10), 
                weighted = FALSE, dmatrix=D1)
    })



    # weighted kNN
    KNN_time_w = system.time({
        my_knn_py(train, test, train_labels_py, k = as.integer(10), 
                weighted = TRUE, dmatrix=D1)
    })
        

RES[xx,1] = MAMI_time[3] #MAMI_perf
RES[xx,2] = KNN_time[3]
RES[xx,3] = KNN_time_w[3]

print(RES)

}