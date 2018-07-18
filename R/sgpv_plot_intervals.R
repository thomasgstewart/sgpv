#' Plot intervals
#'
#' This function plots null and estimation intervals
#' @param data A data.frame or data.table which includes interval bounds for the estimate and the null region.
#' @param estimate_lb Column number or variable name of the estimate interval lower bound.  Default = 1
#' @param estimate_ub Column number or variable name of the estimate interval upper bound.  Default = 2
#' @param null_lb Column number or variable name of the null interval lower bound.  Default = 3
#' @param null_ub Column number or variable name of the null interval upper bound.  Default = 4
#' @param yvar Column number or variable name of the column which indicates the y-value of the interval in the plot.  If NULL, intervals are ploted at integer values corresponding to the negative row number.
#' @param shift Scalar value indicating how much to separate null and estimate intervals vertically.  Value is relative to \code{yvar}.  Default is 0.
#' @param est_col Color of estimate intervals
#' @param null_col Color of null intervals
#' @keywords test_cases_sgpv 
#' @details Generates a plot with intervals. 
#' @return Function invisibly returns a list with the bookend values used in the plot, the pch matrix used for the symbols, and the yvar matrix.
#' @export
#' @examples
#' sgpv_test_cases() %>% sgpv_plot_intervals
#' 

sgpv_plot_intervals <- function(
    data
  , estimate_lb = 1
  , estimate_ub = 2
  , null_lb = 3
  , null_ub = 4
  , yvar = NULL
  , shift = 0
  , est_col = "#d7191c80"
  , null_col = "#2c7bb680"
){
  col_names <- c("el", "eu", "nl", "nu") 
  
  dt <- data %>% 
    as.data.table %>% 
    `[`(, c(estimate_lb, estimate_ub, null_lb, null_ub), with = FALSE) %>% 
    setnames(col_names)
  N <- nrow(dt)
  
  # Remove intervals with any NA
  dt[is.na(el) | is.na(eu),`:=`("el" = NA, "eu" = NA)]
  dt[is.na(nl) | is.na(nu),`:=`("nl" = NA, "nu" = NA)]
  
  # Y values for intervals
  if(is.null(yvar)){
    ys <- -(1:N)
  }else{
    ys <- data[[yvar]]
  }
  
  # Bookend symbols
  pchchar <- function(x){
    out <- -9664*(x %in% c(-Inf)) + #LEFT TRIANGLE
           -9654*(x %in% c(Inf)) +  #RIGHT TRIANGLE
           -124*is.finite(x)        #FINITE BOUND
    out[out == 0] <- NA #NA
    out
  }
  pch_matrix <- dt[,lapply(.SD, pchchar)]
  
  # Circle for points
  pch_matrix[dt[,el] == dt[,eu], `:=`("el" = 16, "eu" = 16)]
  pch_matrix[dt[,nl] == dt[,nu], `:=`("nl" = 16, "nu" = 16)]
  
  # Bookend values
  finite_range <- function(x){ x[is.finite(x) & !is.na(x)] %>% range }
  fr <- dt %>% unlist %>% finite_range
  ir <- fr + c(-1,1)*diff(fr)*.16
  
  bev <- function(x){
    out <- ir[1]*(x %in% c(-Inf)) + #LEFT + 16%
           ir[2]*(x %in% c(Inf))    #RIGHT + 16%
    out[is.finite(x)] <- x[is.finite(x)]           #FINITE value
    out[is.na(x)] <- NA             #NA  
    out
  }
  be <- dt[,lapply(.SD, bev)]
  
  s <- shift/2
  
  plot(ir, range(ys), pch = "", yaxt = "n", ylab = "", xlab = "")
  old_lend <- par()$lend
  par(lend = 1)
  for(i in 1:N){
    lines(be[i,c(el,eu),], c(ys[i], ys[i]) + s, col = est_col, lwd = 4)
    points(be[i,el], ys[i] + s, pch=pch_matrix[i,el], col = est_col)
    points(be[i,eu], ys[i] + s, pch=pch_matrix[i,eu], col = est_col)
    
    lines(be[i,c(nl,nu),], c(ys[i], ys[i]) - s, col = null_col, lwd = 4)
    points(be[i,nl], ys[i] - s, pch=pch_matrix[i,nl], col = null_col)
    points(be[i,nu], ys[i] - s, pch=pch_matrix[i,nu], col = null_col)
  }
  par(lend = old_lend)
  invisible(list(be, pch_matrix, ys))
}
