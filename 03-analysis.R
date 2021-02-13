library(tidyverse)
library(vegan)
library(NbClust)
library(clValid)
library(mclust)


#### Clustering #### 
## Using mahalanobis distance help us dealing with colinearity or strong correlations.
d <- dat %>% select(where(is.numeric), -uncode) %>% 
    vegdist(., method = "mahalanobis", tol= 10e-20)

## Validating number of clusters
# euclidean and manhattan are not good for gradient separation (see help vegdist)
m <- "ward.D2" # minimize the total within-cluster variance

clust_num <- NbClust( 
    data = dat_pro %>% 
        select(where(is.numeric), -uncode),
    #distance = "maximum",
    min.nc = 2, max.nc = 15, 
    method = m, 
    index = 'all') ## 3 is the optimal number

### Stability & Internal validation
stab <- clValid(
    obj = dat_pro %>% 
        select(where(is.numeric), -uncode) %>% 
        as.matrix(), 
    nClust = c(3:9),
    clMethods = c("hierarchical", "kmeans", "diana", "fanny", #"som",
                "model", "sota", "pam", "clara", "agnes"),
    validation = c('stability', "internal"),
    #metric = "manhattan", method = "ward",
    verbose = FALSE
) ## Hierarchical is the optimal method



dat_pro$clus <- clust_num$Best.partition



