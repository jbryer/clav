#' Predict cluster class for k-means cluster
#'
#' @param object results of [stats::kmeans()].
#' @param newdata data frame to make class predictions.
#' @param method method used to make predictions.
#' @param ... currently not used.
#' @return vector of predicted clusters.
#' @export
#' @method predict kmeans
predict.kmeans <- function(
		object,
		newdata,
		method = c("classes", "centers"),
		...
) {
	method <- match.arg(method)
	if(missing(newdata)) {
		newdata <- object$cluster
	}
	centers <- object$centers
	ss_by_center <- apply(centers, 1, function(x) {
		colSums((t(newdata) - x) ^ 2)
	})
	best_clusters <- apply(ss_by_center, 1, which.min)

	if (method == "centers") {
		centers[best_clusters, ]
	} else {
		best_clusters
	}
}

#' Convert vector to z-scores
#' @param x vector to convert.
#' @return vector of standard scores.
#' @export
scale_this <- function(x) {
	(x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

#' Utility function to print p-values
#'
#' This function will print a pretty p-value. If the p-value is less than
#' 10^(-digits), for example 0.01 with digits = 2, then the result will be
#' "p < 0.01".
#'
#' @param p_value the p-value.
#' @param digits number of digits to print.
#' @return a character version of the p-value.
#' @export
print_p_value <- function(p_value, digits = 2) {
	if(p_value < 10^-digits) {
		paste0("p < ", 10^-digits)
	} else {
		paste0("p = ", round(p_value, digits = digits))
	}
}

#' Calculate the Euclidean distance for each observation to the cluster center.
#' @return data frame with the distance to each center.
#' @export
#' @rdname fitmeasures
euclidean_distance <- function(df, centers) {
	if(ncol(centers) != ncol(df)) {
		stop('The number of centers does not appear to match the number of variables')
	}
	dist <- data.frame(row.names = row.names(df))
	k <- nrow(centers)
	for(i in seq_len(k)) {
		dist[,LETTERS[i]] <- 0
		for(j in seq_len(ncol(df))) {
			dist[,LETTERS[i]] <- dist[,LETTERS[i]] + (df[,j] - centers[i,j])^2
		}
		dist[,LETTERS[i]] <- sqrt(dist[,LETTERS[i]])
	}
	return(dist)
}

#' Get the cluster centers.
#' @rdname fitmeasures
#' @export
get_centers <- function(df, clusters) {
	if(length(clusters) != nrow(df)) {
		stop('clusters does not match the number of rows in df.')
	}
	centers <- data.frame(row.names = unique(clusters))
	for(i in names(df)) {
		centers[,i] <- NA
	}
	# for(i in unique(clusters)) {
	for(i in seq_len(length(unique(clusters)))) {
		vars2 <- df[clusters == i,]
		for(j in seq_len(ncol(df))) {
			centers[i,j] <- mean(vars2[,j])
		}
	}
	return(centers)
}

#' Within Sum of Squares
#'
#' @param df data frame with the variables.
#' @param centers the centers for each cluster.
#' @param clusters vector with the cluster membership. This is only used if `centers` is not specified.
#' @rdname fitmeasures
#' @export
wss <- function(df, centers = get_centers(df, clusters), clusters) {
	k <- nrow(centers)
	threshold <- 0.0001
	if(any(abs(apply(df, 2, mean) > threshold)) |
	   any(apply(df, 2, sd) > 1 + threshold | apply(df, 2, sd) < 1 - threshold) ){
		warning('Variables do not appear to be standardized. Estimates may not be correct.')
	}
	# Assign each observation to the closest cluster center
	dist <- euclidean_distance(df, centers)
	clust <- dist |> apply(1, function(x) { which(min(x) == x) })
	wss <- 0
	# Calculate the WSS
	for(i in seq_len(k)) {
		vars2 <- df[clust == i,]
		for(j in seq_len(ncol(df))) {
			wss <- wss + sum((vars2[,j] - centers[i,j])^2)
		}
	}
	return(wss)
}


#' Calculate Silhouette score
#' @rdname fitmeasures
#' @param df data frame to calculate the Silhouette from.
#' @param k number of clusters.
#' @param cluster_fun clustering function.
#' @param ... other parameters passed to [cluster::silhouette()]
#' @importFrom cluster silhouette
#' @importFrom stats kmeans
#' @export
silhouette_score <- function(df, k = 9, cluster_fun = stats::kmeans, ...) {
	ssall <- numeric(length(k))
	for(i in 1:k) {
		km <- cluster_fun(df, i)
		ss <- cluster::silhouette(km$cluster, dist(df), ...)
		ssall[i] <- ifelse(is.na(ss), NA, mean(ss[, 3]))
	}
	return(ssall)
}

#' Calculate Calinski-Harabasz score
#' @rdname fitmeasures
#' @param df data frame to calculate the Calinski-Harabasz from.
#' @param k number of clusters.
#' @param cluster_fun clustering function.
#' @param ... other parameters passed to [fpc::calinhara()]
#' @importFrom fpc calinhara
#' @export
calinski_harabasz <- function(df, k = 9, cluster_fun = stats::kmeans, ...) {
	cal <- numeric(length(k))
	for(i in 1:k) {
		km <- cluster_fun(df, i)
		cal[i] <- fpc::calinhara(df, km$cluster, ...)
	}
	return(cal)
}

#' Calculate Davies-Bouldin score
#' @rdname fitmeasures
#' @param df data frame to calculate the Davies-Bouldin from.
#' @param k number of clusters.
#' @param cluster_fun clustering function.
#' @param ... other parameters passed to [clusterSim::index.DB()]
#' @importFrom stats kmeans
#' @importFrom clusterSim index.DB
#' @export
davies_bouldin <- function(df, k = 9, cluster_fun = stats::kmeans, ...) {
	davies <- numeric(length(k))
	for(i in 1:k) {
		km <- cluster_fun(df, i)
		db <- clusterSim::index.DB(df, km$cluster, ...)
		davies[i] <- db$DB
	}
	return(davies)
}

#' Calculate Rand index
#' @rdname fitmeasures
#' @param df data frame to calculate the Rand index from.
#' @param k number of clusters.
#' @param cluster_fun clustering function.
#' @param ... other parameters passed to [fossil::rand.index()].
#' @importFrom fossil rand.index
#' @importFrom stats fitted kmeans
#' @export
rand_index <- function(df, k = 9, cluster_fun = stats::kmeans, ...) {
	rand <- numeric(length(k))
	rand[1] <- NA
	# out1 <- cluster_fun(df, 1) |> stats::fitted(method = 'classes')
	out1 <- cluster_fun(df, 1)$cluster
	for(i in 2:k) {
		# out2 <- cluster_fun(df, i) |> stats::fitted(method = 'classes')
		out2 <- cluster_fun(df, i)$cluster
		rand[i] <- fossil::rand.index(out1, out2)
		out1 <- out2
	}
	return(rand)
}

