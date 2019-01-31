#' Second Generation p-values
#'
#' This function calculates p_δ and δ-gap.
#' @param data A data.frame or data.table which includes interval bounds for the estimate and the null region.
#' @param estimate_lb Column number or variable name of the estimate interval lower bound.  Default = 1
#' @param estimate_ub Column number or variable name of the estimate interval upper bound.  Default = 2
#' @param null_lb Column number or variable name of the null interval lower bound.  Default = 3
#' @param null_ub Column number or variable name of the null interval upper bound.  Default = 4
#' @param epsilon A (usually) small scalar to denote positive but infinitesimally small p_δ.  Default = 1e-5.  Values that are infinitesimally close to 1 are assigned \code{1-epsilon}.
#' @keywords test_cases_sgpv 
#' @export
#' @examples
#' sgpv()
#' 

sgpv <- function(data, estimate_lb = 1, estimate_ub = 2, null_lb = 3, null_ub = 4, epsilon = 1e-5){
  
  col_names <- c("el", "eu", "nl", "nu") 
  
  dt <- data %>% 
    as.data.table %>% 
    `[`(, c(estimate_lb, estimate_ub, null_lb, null_ub), with = FALSE) %>% 
    setnames(col_names)

  dt[, `:=`("case" = NA_real_, "p_delta" = NA_real_, "delta_gap" = NA_real_)]

  # CASE 1: Has NAs
  dt[, case := is.na(.SD) %>% rowSums %>% `>`(0) %>% `*`(1), .SDcols = col_names]
  dt[case == 1, `:=`("p_delta" = NA_real_, "delta_gap" = NA)]
  
  
  # CASE 2: Wrong Ordering
  dt[case == 0, case := 2 * ((el > eu) | (nl > nu))]
  dt[case == 2, `:=`("p_delta" = NA, "delta_gap" = NA)]
  
  
  # CASE 3: All finite
  dt[case == 0, case := .SD %>% sapply(is.finite) %>% rowSums %>% `==`(4) %>% `*`(3), .SDcols = col_names]
    ### calc estimate and null length
    dt[case == 3, `:=`("elen" = eu - el, "nlen" = nu - nl)]

      
    ### CASE 3.1: Point Null Interval & Point Estimate Interval 
    dt[case == 3 & elen == 0 & nlen == 0, case := 3.1]
      ### Case 3.11: Point Null Interval == Point Estimate Interval
      dt[case == 3.1 & (el == nl), `:=`("p_delta" = 1, "delta_gap" = 0, "case" = 3.11)]
      ### Case 3.12: Point Null Interval != Point Estimate Interval
      dt[case == 3.1 & (el != nl), `:=`("p_delta" = 0, "delta_gap" = el - nl, "case" = 3.12)]

            
    ### CASE 3.2: Point Null Interval
    dt[case == 3 & nlen == 0, case := 3.2]
      ### CASE 3.21: Overlap    [--- . ---]
      dt[case == 3.2 & (el <= nl) & (eu >= nl), `:=`("p_delta" = 1/2, "delta_gap" = 0, "case" = 3.21)]
      ### CASE 3.22:  Point to the left   . [------]   
      dt[case == 3.2 & (nl < el), `:=`("p_delta" = 0, "delta_gap" = el - nl, "case" = 3.22)]
      ### CASE 3.23:  Point to the right   [------] .
      dt[case == 3.2 & (eu < nl), `:=`("p_delta" = 0, "delta_gap" = eu - nl, "case" = 3.23)]
      

    ### CASE 3.3: Point Estimate Interval
    dt[case == 3 & elen == 0, case := 3.3]
      ### CASE 3.31: Overlap    [--- . ---]
      dt[case == 3.3 & (nl <= el) & (eu <= nu), `:=`("p_delta" = 1, "delta_gap" = 0, "case" = 3.31)]
      ### CASE 3.32: Point to the left   . [------]   
      dt[case == 3.3 & (eu < nl), `:=`("p_delta" = 0, "delta_gap" = eu - nl, "case" = 3.32)]
      ### CASE 3.33: Point to the right   [------] .
      dt[case == 3.3 & (nu < el), `:=`("p_delta" = 0, "delta_gap" = eu - nu, "case" = 3.33)]
    
    
    ### Case 3.4: The usual cases
    dt[case == 3, `:=`("case" = 3.4, denom = pmin(2*nlen, elen))]
    
      ### CASE 3.41:
      ### E      [-----]
      ### N   [-----]
      dt[case == 3.4 & (nl <= el) & (el <= nu) & (nu <= eu), `:=`("p_delta" = (nu - el)/denom, "delta_gap" = 0, "case" = 3.41)]
      
      ### CASE 3.42:
      ### E   [-----]
      ### N      [-----]
      dt[case == 3.4 & (el <= nl) & (nl <= eu) & (eu <= nu), `:=`("p_delta" = (eu - nl)/denom, "delta_gap" = 0, "case" = 3.42)]
      
      ### CASE 3.43:
      ### E            [-----]
      ### N   [-----]
      dt[case == 3.4 & (nu < el), `:=`("p_delta" = 0, "delta_gap" = el - nu, "case" = 3.43)]
      
      ### CASE 3.44:
      ### E   [-----]
      ### N            [-----]
      dt[case == 3.4 & (eu < nl), `:=`("p_delta" = 0, "delta_gap" = eu - nl, "case" = 3.44)]
      
      ### CASE 3.45:
      ### E     [--]
      ### N   [------]
      dt[case == 3.4 & (nl <= el) & (eu <= nu), `:=`("p_delta" = 1, "delta_gap" = 0, "case" = 3.45)]
      
      ### CASE 3.46:
      ### E   [-----]
      ### N    [--]
      dt[case == 3.4 & (el <= nl) & (nu <= eu), `:=`("p_delta" = (nu - nl)/denom, "delta_gap" = 0, "case" = 3.46)]
      
    
  # Case 4: The Infinite Null Interval and Esimate Interval are both infinite
  dt[case == 0 & (is.infinite(nl) | is.infinite(nu)) & (is.infinite(el) | is.infinite(eu)), case := 4]
  
    ### Case 4.1: 
    ### E    <------->
    ### N    <------->
    dt[case == 4 & is.infinite(nl) & is.infinite(nu) & is.infinite(el) & is.infinite(eu), `:=`("p_delta" = .5, "delta_gap" = 0, "case" = 4.1)]
  
    ### Case 4.2:
    ### E      [------->
    ### N   <-----]
    dt[case == 4 & is.infinite(nl) & (el >= nu) & is.infinite(eu), `:=`("p_delta" = epsilon, "delta_gap" = 0, "case" = 4.2)]
    
    ### Case 4.3
    ### E   <-----]
    ### N      [------>
    dt[case == 4 & is.infinite(el) & (nl <= eu) & is.infinite(nu), `:=`("p_delta" = epsilon, "delta_gap" = 0, "case" = 4.3)]
    
    ### Case 4.4
    ### E            [------->
    ### N   <-----]
    dt[case == 4 & is.infinite(nl) & (nu >= el) & is.infinite(eu), `:=`("p_delta" = 0, "delta_gap" = el - nu, "case" = 4.4)]
    
    ### Case 4.5
    ### E   <-----]
    ### N            [------>
    dt[case == 4 & is.infinite(el) & (eu >= nl) & is.infinite(nu), `:=`("p_delta" = 0, "delta_gap" = eu - nl, "case" = 4.5)]
    
    ### Case 4.6
    ### E   [------->
    ### N      [------->
    dt[case == 4 & (el <= nl) & is.infinite(nu) & is.infinite(eu), `:=`("p_delta" = 1 - epsilon, "delta_gap" = 0, "case" = 4.6)]

    ### Case 4.7
    ### E      [------->
    ### N   [------->
    dt[case == 4 & (nl <= el) & is.infinite(nu) & is.infinite(eu), `:=`("p_delta" = 1, "delta_gap" = 0, "case" = 4.7)]

        
  # Case 5: The Infinite Null Interval
  dt[case == 0 & (is.infinite(nl) | is.infinite(nu)), case := 5]
  
    ### Case 5.1
    ### E      [---]  
    ### N   <-------->
    dt[case == 5 & is.infinite(nl) & is.infinite(nu), `:=`("p_delta" = 1, "delta_gap" = 0, "case" = 5.1)]

    ### Case 5.2
    ### E      [---]  
    ### N   [-------->
    dt[case == 5 & is.finite(nl) & (nl <= el) & is.infinite(nu), `:=`("p_delta" = 1, "delta_gap" = 0, "case" = 5.2)]
    
    ### Case 5.3
    ### E   [----]  
    ### N     [-------->
    dt[case == 5 & is.finite(nl) & (el <= nl) & (nl <= eu) & is.infinite(nu), `:=`("p_delta" = (eu - nl)/(eu - el), "delta_gap" = 0, "case" = 5.3)]
    
    ### Case 5.4
    ### E   [---]  
    ### N         [-------->
    dt[case == 5 & is.finite(nl) & (eu <= nl) & is.infinite(nu), `:=`("p_delta" = 0, "delta_gap" = nl - eu, "case" = 5.4)]
    
    ### Case 5.5
    ### E      [---]  
    ### N   <--------]
    dt[case == 5 & is.infinite(nl) & (eu <= nu), `:=`("p_delta" = 1, "delta_gap" = 0, "case" = 5.5)]
    
    ### Case 5.6
    ### E      [----]  
    ### N   <----]
    dt[case == 5 & is.infinite(nl) & (el <= nu) & (nu <= eu), `:=`("p_delta" = (eu - nu)/(eu - el), "delta_gap" = 0, "case" = 5.6)]
    
    ### Case 5.7
    ### E              [---]  
    ### N   <-------]
    dt[case == 5 & is.infinite(nl) & (nu <= el), `:=`("p_delta" = 0, "delta_gap" = el - nu, "case" = 5.7)]
    

  # Case 6: The Infinite Estimate Interval
  dt[case == 0 & (is.infinite(el) | is.infinite(eu)), case := 6]
    
    ### Case 6.1
    ### E   <-------->
    ### N      [---]  
    dt[case == 6 & is.infinite(el) & is.infinite(eu), `:=`("p_delta" = 0.5, "delta_gap" = 0, "case" = 6.1)]
    
    ### Case 6.2
    ### E   [-------->
    ### N      [---]
    dt[case == 6 & is.finite(el) & (el <= nl) & is.infinite(eu), `:=`("p_delta" = 0.5, "delta_gap" = 0, "case" = 6.2)]
    
    ### Case 6.3
    ### E      [-------->
    ### N   [----]
    dt[case == 6 & is.finite(el) & (nl <= el) & (el <= nu) & is.infinite(eu), `:=`("p_delta" = (nu - el)/(nu - nl)/2, "delta_gap" = 0, "case" = 6.3)]
    
    ### Case 6.4
    ### E          [-------->
    ### N   [---] 
    dt[case == 6 & is.finite(el) & (nu <= el) & is.infinite(nu), `:=`("p_delta" = 0, "delta_gap" = el - nu, "case" = 6.4)]
    
    ### Case 6.5
    ### E   <--------]
    ### N      [---]  
    dt[case == 6 & is.infinite(el) & (nu <= eu), `:=`("p_delta" = 0.5, "delta_gap" = 0, "case" = 6.5)]
    
    ### Case 6.6
    ### E   <----]
    ### N      [----]  
    dt[case == 6 & is.infinite(el) & (nl <= eu) & (eu <= nu), `:=`("p_delta" = (eu - nl)/(nu - nl)/2, "delta_gap" = 0, "case" = 6.6)]
    
    ### Case 6.7
    ### E   <-------]
    ### N              [---]
    dt[case == 6 & is.infinite(el) & (eu <= nl), `:=`("p_delta" = 0, "delta_gap" = eu - nl, "case" = 6.7)]
    
  dt[,`:=`("elen" = NULL, "nlen" = NULL, "denom" = NULL)]
  dt
}
