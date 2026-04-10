This study will compare a bootstrapping approach to estimating the optimal number of
clusters to existing measures (see 1). The bootstrapping method works by taking m bootstrap
samples, estimating a k-means model for each bootstrap sample, and recording the mean for each
variable and cluster. The result is a collection of distributions for each cluster and variable
combination. From those distributions the percent overlap is calculated. The overlap fit metric is
the mean or median overlap of all variable and cluster combinations.
