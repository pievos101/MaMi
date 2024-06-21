# MaMi

## Calibrated kNN Classification via Second-Layer Neighborhood Analysis 

<p align="center">
<img src="https://github.com/pievos101/MaMi/blob/main/logo.jpeg" width="350">
</p>


## Installation
The MaMi R-package can be installed using devtools.

```{r}
install.packages("devtools")
library(devtools)

devtools::install_github("pievos101/MaMi")
library(MaMi)
```

## Load from source

The development of the MaMi R-package is in progress. Meanwhile the Major-Minority algorithm can be loaded via source file.

```{r}
source("mami.r")
```
## Usage

```{r}
# Load data
data(iris)
data = as.matrix(iris[,1:4])
target = iris[,5]

# Train-test split 

## 80% of the sample size
smp_size <- floor(0.80 * nrow(data))

train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test  <- data[-train_ind, ]

target_train = target[train_ind]
target_test  = target[-train_ind]

# Call the Major-Minority algorithm
source("mami.r")
res = mami(train, test, target_train)
res
```
