#' Basic summary statistics by group
#'
#' Calculate basic summary statistics by a grouping variable. This function is inspired by the
#' `psych::describeBy()` function but has been implemented to always return a data frame using
#' the tidyverse. The `funs` parameter provides additional flexibility regarding what summary
#' statistics are calculated.
#'
#' @param df a data frame to summarize.
#' @param group either a character vector of column names in `df` to group by or vector where the
#'        the length is equal to the number of rows in `df`.
#' @param group_name if `group` is a grouping vector then `group_name` will the name used in the
#'        returned data frame. Otherwise, `group_name` will be prepended to the returned variables
#'        representing the grouping variables (i.e. from `group`).
#' @param funs a named list of functions. The names of the list will be used as the column names
#'        in the returned data frame.
#' @param var_sep variable separator.
#' @return a data frame with a column `group` corresponding to the group variable and column
#'         `variable` corresponding to the names of the data frame. Additional columns for each
#'         function in the `funs` parameter.
#' @export
#' @importFrom dplyr mutate group_by summarise select all_of across
#' @importFrom reshape2 melt dcast
#' @importFrom tidyr separate_wider_delim
#' @examples
#' data(pisa2015, package = 'clav')
#' describe_by(df = pisa2015[,c('science_score', 'efficacy', 'motivation', 'country')], group = 'country')
describe_by <- function(
		df,
		group,
		group_name = 'group',
		var_sep = '__',
		funs = list(n = length,
					mean = mean,
					sd = sd,
					se = function(x, ...) { sd(x, ...) / sqrt(length(x)) },
					median = median,
					min = min,
					max = max,
					range = function(x, ...) { diff(range(x, ...)) })
) {
	if(length(grep(var_sep, names(df))) > 0) {
		stop(paste0('Cannot use ', var_sep, ' as variable separator since it is already used.'))
	}
	if(nrow(df) == length(group)) {
		renaming <- c('group')
		names(renaming) <- group_name
		df <- df |>
			dplyr::mutate(group = group) |>
			dplyr::rename(renaming)
		group <- group_name
	} else {
		names(group) <- paste0(group_name, var_sep, group)
		df <- df |>	dplyr::rename(group)
		group <- names(group)
	}
	var_check <- apply(df[,group, drop=FALSE], 2, FUN = function(x) { grep(var_sep, x, fixed = TRUE) })
	if(length(var_check) > 0) {
		stop(paste0('Cannot use ', var_sep, ' as variable separator since it is already used.'))
	}

	summary_cols <- names(df)[!(names(df) %in% group)]
	cast_formula <- paste0(paste0(group, collapse = ' + '), ' + variable ~ fun') |> as.formula()
	df |>
		dplyr::group_by(dplyr::across(dplyr::all_of(group))) |>
		dplyr::summarise(dplyr::across(summary_cols, .fns = funs, .names = paste0("{.col}", var_sep, "{.fn}"))) |>
		reshape2::melt(id.vars = group) |>
		tidyr::separate_wider_delim(cols = variable,
									delim = var_sep,
									names = c('variable', 'fun')) |>
		reshape2::dcast(cast_formula, value.var = 'value')
}
