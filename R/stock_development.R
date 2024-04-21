#' Stock Development
#' @noRd
stock_development <- function(S, n, u, d){
  if(n == 0){
    S_n <- S
  }else{
    S_n <- c(
      stock_development(n = n - 1, u = u, d = d, S = S)*d,
      stock_development(n = n - 1, u = u, d = d, S = S)*u
    )
  }
  return(S_n)
}

