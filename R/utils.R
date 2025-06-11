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

#' Calculate within sum of squares.
#' @rdname fitmeasures
#' @param df data frame to calculate the WSS from.
#' @param k number of clusters.
#' @param cluster_fun clustering function.
#' @importFrom stats kmeans
#' @export
wss <- function(df, k = 9, cluster_fun = stats::kmeans) {
	wss <- (nrow(df) - 1) * sum(apply(df, 2, var))
	for(i in 1:k) {
		wss[i] <- sum(cluster_fun(df, centers = i)$withinss)
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
		km <- cluster_fun(df, centers = i, nstart = 25)
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
		km <- kmeans(df, i)
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
	out1 <- cluster_fun(df, 1) |> stats::fitted(method = 'classes')
	for(i in 2:k) {
		out2 <- cluster_fun(df, i) |> stats::fitted(method = 'classes')
		rand[i] <- fossil::rand.index(out1, out2)
		out1 <- out2
	}
	return(rand)
}

