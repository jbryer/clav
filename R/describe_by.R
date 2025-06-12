#' Basic summary statistics by group
#'
#' Calculate basic summary statistics by a grouping variable. This function is inspired by the
#' `psych::describeBy()` function but has been implemented to always return a data frame using
#' the tidyverse. The `funs` parameter provides additional flexibility regarding what summary
#' statistics are calculated.
#'
#' @param df a data frame to summarize.
#' @param group a grouping vector.
#' @param funs a named list of functions. The names of the list will be used as the column names
#'        in the returned data frame.
#' @param group_name name of the grouping variable in the returned data frame.
#' @return a data frame with a column `group` corresponding to the group variable and column
#'         `variable` corresponding to the names of the data frame. Additional columns for each
#'         function in the `funs` parameter.
#' @export
#' @importFrom dplyr mutate group_by summarise select all_of across
#' @importFrom reshape2 melt dcast
#' @importFrom tidyr separate_wider_delim
#' @examples
#' data(pisa2015, package = 'clav')
#' describe_by(df = pisa2015[,c('science_score', 'efficacy', 'motivation')], group = pisa2015$country)
describe_by <- function(
		df,
		group,
		group_name = 'group',
		funs = list(n = length,
					mean = mean,
					sd = sd,
					se = function(x, ...) { sd(x, ...) / sqrt(length(x)) },
					median = median,
					min = min,
					max = max,
					range = function(x, ...) { diff(range(x, ...)) })
) {
	if(nrow(df) != length(group)) {
		stop('Number of rows in the data frame must be the same length as the grouping variable.')
	}
	rename <- 'group'
	names(rename) <- group_name
	var_sep <- '_'
	while(length(grep(var_sep, names(df))) > 0) {
		var_sep <- paste0(var_sep, '_')
	}
	df |>
		dplyr::mutate(group = group) |>
		dplyr::group_by(group) |>
		dplyr::summarise(dplyr::across(names(df), .fns = funs, .names = paste0("{.col}", var_sep, "{.fn}"))) |>
		reshape2::melt(id.vars = 'group') |>
		tidyr::separate_wider_delim(cols = variable,
									delim = var_sep,
									names = c('variable', 'fun')) |>
		reshape2::dcast(group + variable ~ fun, value.var = 'value') |>
		dplyr::rename(dplyr::all_of(rename)) |>
		dplyr::select(dplyr::all_of(c(group_name, 'variable', names(funs))))
}
