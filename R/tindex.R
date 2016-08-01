#' Simple function to create intra-group ranking and count variables.
#'
#' This function creates an index or count variable by group, optionally sorting on an additional parameter of interest.
#' For example, one may create an index/ranking of income within some group, e.g. the median income of census tracts within counties of a state. Or, one may create
#' a static count within some grouping variable, e.g. class size within a dataframe of test scores.
#'
#'
#' @param df character; an R \code{\link{data.frame}}
#' @param grp character; a grouping variable, such as a subject number, a classroom, a county, etc.
#' @param sortvar character; sorts the data within grp (see above) by an additional variable (e.g., mpg within cyl in matcars). If no variable df is specified, data are sorted by grp.
#' @param desc logical; when \code{TRUE}, sortvar is sorted in descending order within grp.
#' @param output character; what type of output is desired, a within group "index" (the default) or a "count" within grp?
#'
#' @examples
#' # Build a count variable by number of cylinders, returning a vector:
#' mtcars$cylinder_count <- tindex(mtcars, cyl, output="count")
#'
#' # Build a within-type index sorting by a second variable:
#' mtcars$ranked <- tindex(mtcars, cyl, sortvar=mpg, output="index", desc=TRUE)

#' @export


tindex  <-
	function(df, grp, sortvar = NULL, output = "index", desc=FALSE) {
		set.seed(1234)
		args <- match.call()
		df$.oindex <- 1:nrow(df)
		sortin <- ("sortvar" %in% names(args))
		namein <- ("name" %in% names(args))

		if(!output %in% c("count", "index"))
			stop("Output must be either 'index' or 'count'!")

		if("grp" %in% names(args) == FALSE)
			stop("No grouping variable defined!")

		if("df"  %in% names(args) == FALSE)
			stop("No data frame defined!")

		if(!sortin & desc)
			warning("Descending sort is TRUE without a sortvar, ignoring this parameter!")

		if(sortin)
			df$.sortadd <- eval(substitute(sortvar), df)

		df$.grp <- eval(substitute(grp), df)

		if(sortin & desc){
			df <- df[with(df, order(.grp, -.sortadd)),]
			} else if (sortin & !desc){
			df <- df[with(df, order(.grp, .sortadd)),]
			} else {
			df <- df[with(df, order(.grp)),]
		}
		if(output == "count")
			df$final <- with(df, ave(.grp, .grp, FUN = length))
		if(output == "index")
			df$final <- with(df, ave(rep(1, nrow(df)), .grp, FUN = seq_along))

		df <- df[with(df, order(.oindex)),]
		return(df$final)
}



