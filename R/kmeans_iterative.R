#' Function to perform K-means step by step and save iterations
#'
#' This function performs K-means clustering but saves the cluster assignment and centers for each
#' iteration of the algorithm.
#'
#' @param x numeric matrix of data, or an object that can be coerced to such a matrix (such as a
#'        numeric vector or a data frame with all numeric columns).
#' @param centers either a numeric value for k or a k by ncol(x) matrix of the initial centers.
#' @param max_iter maximum number of iterations before stopping if convergence doesn't occur first.
#' @return a list where each element corresponds to an interation in the k-means algorithm. For each
#'        element there are three values: `iteration` (numeric for the which iteration), `centers`
#'        (matrix for the cluster centers), and `clusters` (vector of cluster assignment for all
#'        observations in `x`).
#' @export
#' @importFrom stats aggregate
kmeans_iterative <- function(x, centers, max_iter = 50) {
	# Initialize list to store results from each iteration
	iteration_results <- list()

	if(length(centers) == 1) {
		centers <- x[sample(nrow(x), centers), ]
	}

	# Ensure 'x' is a matrix
	x <- as.matrix(x)
	centers <- as.matrix(centers)

	for (i in 1:max_iter) {
		# 1. Assignment step: assign each observation to the nearest center
		# Calculate distances (Euclidean distance is standard)
		distances <- as.matrix(dist(rbind(x, centers)))
		# Extract only distances from data points to centers
		dist_to_centers <- distances[1:nrow(x), (nrow(x) + 1):ncol(distances)]

		# Find the closest center for each point
		clusters <- apply(dist_to_centers, 1, which.min)

		# 2. Update step: calculate new centers as the mean of the assigned points
		new_centers <- stats::aggregate(x, by = list(clusters), FUN = mean)[, -1]

		# Save the results of the current iteration
		iteration_results[[i]] <- list(
			iteration = i,
			centers = new_centers,
			clusters = clusters
		)

		# Check for convergence (if centers stop changing significantly)
		if (all(abs(new_centers - centers) < 1e-6)) {
			cat(paste("Converged at iteration", i, "\n"))
			break
		}

		# Update centers for the next iteration
		centers <- new_centers
	}

	return(iteration_results)
}
