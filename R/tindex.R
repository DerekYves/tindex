#' Simple function to create intra-group ranking and count variables.
#'
#' This function creates an index or count variable by group, optionally sorting on an additional parameter of interest.
#' For example, one may create an index/ranking of income within some group, e.g. the median income of census tracts within counties of a state. Or, one may create
#' a static count within some grouping variable, e.g. class size within a dataframe of test scores.
#'
#'
#' @param df An R \code{\link{data.frame}}
#' @param grp A grouping variable, such as a subject number, a classroom, a county, etc.
#' @param sortvar Sorts the data within grp (see above) by an additional variable (e.g., mpg within cyl in matcars). If no variable df is specified, data are sorted by grp.
#' @param desc When true, sortvar becomes a descending sort within grp.
#' @param output What type of output is desired, a within group "index" (the default) or a "count" within grp?
#' @param vector When true (the default), an index or count vector is returned which matches the sorting of df. When false, a data.frame sorted by grp and, optionally, sortvar, is returned with an additional count or index or count column.
#' @param name Ovveride the default column names returned when vector is false ("index" or "count").
#'
#' Examples:
#'
#' Build a count variable by number of cylinders, returning a vector:
#' mtcars$cylinder_count <- tindex(mtcars, cyl, output="count")
#'
#' Build a mile per gallon index by number of cylinders, returning a sorted data frame with variable "cyl_rank":
#' ranked <- tindex(mtcars, cyl, sortvar=mpg, output="index", vector=F, name="cyl_rank")
#'
#' Same as the previous example but reverses the order of "cyl_rank":
#' ranked <- tindex(mtcars, cyl, sortvar=mpg, output="index", vector=F, name="cyl_rank", desc=T)



tindex  <-
	function(df, grp, sortvar = NULL, output = "index", vector=T, name=NULL, desc=F) {
		set.seed(1234)
		theArgs <- match.call()
		df$.oindex <- 1:nrow(df)
		sortin <- ("sortvar" %in% names(theArgs))
		namein <- ("name" %in% names(theArgs))
		if(namein)  toassign <- name
		if(!namein) toassign <- output
		if("grp" %in% names(theArgs) == F)
			warning("No grouping variable defined!")
		if("df"  %in% names(theArgs) == F)
			warning("No data frame defined!")
		if(!sortin & desc) warning("Descending sort is TRUE without a sortvar, ignoring this parameter!")
		stopifnot("grp" %in% names(theArgs) | "df" %in% names(theArgs))
		if(sortin)
			df$.sortadd <- eval(substitute(sortvar), df)
		df$.grp     <- eval(substitute(grp), df)
		if(sortin & desc){
			df <- df[with(df, order(.grp, -.sortadd)),]
		} else if (sortin & !desc){
			df <- df[with(df, order(.grp, .sortadd)),]
		} else {
			df <- df[with(df, order(.grp)),]
		}
		if(output == "count")
			df$.var <- with(df, ave(.grp, .grp, FUN = length))
		if(output == "index")
			df$.var <- with(df, ave(rep(1, nrow(df)), .grp, FUN = seq_along))
		if(vector==T) {
			df <- df[with(df, order(.oindex)),]
			final <- df$.var
		} else {
			names(df)[names(df)==".var"] <- toassign
			df$.oindex <- df$.sortadd <- df$.grp <- NULL
			final <- df
		}
		return(final)
}



