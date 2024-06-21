# MaMi

## Calibrated kNN Classification via Second-Layer Neighborhood Analysis 

<p align="center">
<img src="https://github.com/pievos101/MaMi/blob/main/logo.jpeg" width="200">
</p>


## Installation
The MaMi R-package can be installed using devtools.

```{r}
install.packages("devtools")
library(devtools)

devtools::install_github("pievos101/MaMi")
library(cpath)

```

## Usage

```{r}
library(MaMi)

# Generate simulated data
res  = sim()
data = res$data
target = res$target

# Train-test split 

## 80% of the sample size
smp_size <- floor(0.80 * nrow(data))

train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test  <- data[-train_ind, ]

target_train = target[train_ind]
target_test  = target[-train_ind]

```
