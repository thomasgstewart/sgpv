#' Test cases for sgpv
#'
#' This function calculates generates test cases for \code{sgpv}
#' @keywords sgpv
#' @export
#' @examples
#' sgpv_test_cases()
#' 

sgpv_test_cases <- function(){
  `%r%` <- function(a, b) rbind(a,b)
  out <- 
  # CASE 1: Has NAs
  c(NA, 2, 3, 4)   %r%
  # CASE 2: Wrong Ordering
  c(NA, 2, 4, 3)   %r%
  # CASE 3: All finite
  ### CASE 3.1: Point Null Interval & Point Estimate Interval 
  ### Case 3.11: Point Null Interval == Point Estimate Interval
  c(2,2,2,2)  %r%
  ### Case 3.12: Point Null Interval != Point Estimate Interval
  c(2,2,3,3)   %r% 
  c(3,3,2,2)   %r%
  ### CASE 3.2: Point Null Interval
  ### CASE 3.21: Overlap    [--- . ---]
  c(2,4,3,3)   %r%
  ### CASE 3.22:  Point to the left   . [------]
  c(2,4,1,1)   %r%
  ### CASE 3.23:  Point to the right   [------] .
  c(2,4,5,5)   %r%
  ### CASE 3.3: Point Estimate Interval
  ### CASE 3.31: Overlap    [--- . ---]
  c(3,3,2,4)   %r%
  ### CASE 3.32: Point to the left   . [------] 
  c(1,1,2,4)   %r%
  ### CASE 3.33: Point to the right   [------] .
  c(5,5,2,4)   %r%
  ### Case 3.4: The usual cases
  ### CASE 3.41:
  ### E      [-----]
  ### N   [-----]
  c(4,6,2,5)   %r%
  ### CASE 3.42:
  ### E   [-----]
  ### N      [-----]
  c(2,5,4,6)   %r%
  ### CASE 3.43:
  ### E            [-----]
  ### N   [-----]
  c(5,6,1,2)   %r%
  ### CASE 3.44:
  ### E   [-----]
  ### N            [-----]
  c(1,2,3,4)   %r%
  ### CASE 3.45:
  ### E     [--]
  ### N   [------]
  c(2,3,1,5)   %r%
  ### CASE 3.46:
  ### E   [-----]
  ### N    [--]
  c(1,5,2,3)   %r%
  # Case 4: The Infinite Null Interval and Esimate Interval are both infinite
  ### Case 4.1: 
  ### E    <------->
  ### N    <------->
  c(-Inf, Inf, -Inf, Inf)    %r%
  ### Case 4.2:
  ### E      [------->
  ### N   <-----]
  c(1, Inf, -Inf, 2)   %r%
  ### Case 4.3
  ### E   <-----]
  ### N      [------>
  c(-Inf, 1, 0, Inf)   %r%
  ### Case 4.4
  ### E            [------->
  ### N   <-----]
  c(1, Inf, -Inf, 0)   %r%
  ### Case 4.5
  ### E   <-----]
  ### N            [------>
  c(-Inf,0,1,Inf)   %r%
  ### Case 4.6
  ### E   [------->
  ### N      [------->
  c(1, Inf, 2, Inf)   %r%
  ### Case 4.7
  ### E      [------->
  ### N   [------->
  c(2, Inf, 1, Inf)   %r%
  # Case 5: The Infinite Null Interval
  ### Case 5.1
  ### E      [---]  
  ### N   <-------->
  c(1,2,-Inf, Inf)   %r%
  ### Case 5.2
  ### E      [---]  
  ### N   [-------->
  c(2,3,1,Inf)   %r%
  ### Case 5.3
  ### E   [----]  
  ### N     [-------->
  c(2,4,3,Inf)   %r%
  ### Case 5.4
  ### E   [---]  
  ### N         [-------->
  c(1,2,3,Inf)   %r%
  ### Case 5.5
  ### E      [---]  
  ### N   <--------]
  c(1,2,-Inf,3)   %r%
  ### Case 5.6
  ### E      [----]  
  ### N   <----]
  c(1,3,-Inf,2)   %r%
  ### Case 5.7
  ### E              [---]  
  ### N   <-------]
  c(2,3,-Inf,1)   %r%
  # Case 6: The Infinite Estimate Interval
  ### Case 6.1
  ### E   <-------->
  ### N      [---]
  c(-Inf,Inf,1,2)   %r%
  ### Case 6.2
  ### E   [-------->
  ### N      [---]
  c(1, Inf, 2, 3)   %r%
  ### Case 6.3
  ### E      [-------->
  ### N   [----]
  c(2, Inf, 1, 3)   %r%
  ### Case 6.4
  ### E          [-------->
  ### N   [---]
  c(3, Inf, 1, 2)   %r%
  ### Case 6.5
  ### E   <--------]
  ### N      [---]  
  c(-Inf,3,1,2)   %r%
  ### Case 6.6
  ### E   <----]
  ### N      [----] 
  c(-Inf,3,1,4)   %r%
  ### Case 6.7
  ### E   <-------]
  ### N              [---]
  c(-Inf, 1, 2, 3)
  
  out <- data.frame(out, row.names = NULL)
  setNames(out, c("estimate_lb","estimate_ub","null_lb","null_ub"))
}
