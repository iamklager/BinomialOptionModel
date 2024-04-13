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

Derivative_Security = function(S, K, N, r, d, u, type){
  stock_tree = stock_development(S, N, u, d)
  q = ((1+r) - d)/(u - d)
  option_tree = matrix(0, nrow = N+1, ncol = length(stock_tree))
  deltas = matrix(0, nrow = N, ncol = length(stock_tree)/2)
  
  
  if(type == "European_call"){
    option_tree[nrow(option_tree),] = pmax(stock_tree - K,0)
  }
  
  else if(type == "European_put"){
    option_tree[nrow(option_tree),] = pmax(K - stock_tree,0)
  }
  
  else if(type == "Asian_call"){
    for ( i in 0:N) {
      if(i == 0) {
        temp <- S
      } else {
        temp <- (stock_development(S, i , u , d) + rep(temp , each = 2))
      }
    }

    option_tree[nrow(option_tree),] = pmax((temp /( N + 1))- K,0)
  }
  
  else if(type == "Asian_put"){
    for ( i in 0:N) {
      if(i == 0) {
        temp <- S
      } else {
        temp <- (stock_development(S, i , u , d) + rep(temp , each = 2))
      }
    }
    
    option_tree[nrow(option_tree),] = pmax(K - (temp /( N + 1)),0)
  }
  
  else if(type == "American_call"){
    
    option_tree[nrow(option_tree),] = pmax(stock_tree - K,0)
    
    for(i in (nrow(option_tree)):2){
      option_tree[i-1, 1:(2^(N-1))] <-
        (1/(1+r))*(q*option_tree[i, seq(2, ncol(option_tree), by = 2)] + (1-q)*option_tree[i, seq(1, ncol(option_tree), by = 2)])
      
      tmp1 <- option_tree[i-1, 1:(2^(N-1))]
      tmp2 <- rep(0,length(tmp1))
      
      if((i-1) >= 1){
        tmp2[1:length(stock_development(S, i-2, u, d))] <- pmax(stock_development(S, i-2, u, d)-K, 0)
      }
      
      option_tree[i-1, 1:(2^(N-1))] <- pmax(tmp1, tmp2)      
      stock_row = stock_development(S, i-1, u, d)
      deltas[i-1, 1:(2^(N-1))] <- (option_tree[i, seq(2, ncol(option_tree), by = 2)] - option_tree[i, seq(1, ncol(option_tree), by = 2)])/(stock_row[seq(2, ncol(option_tree), by = 2)] - stock_row[seq(1, ncol(option_tree), by = 2)])
    }
    
    deltas[is.na(deltas)] <- 0
    
    return(list(derivative_prices = option_tree, delta = deltas))
  }    
  
  else if(type == "American_put"){
    
    option_tree[nrow(option_tree),] = pmax(K - stock_tree,0)
    
    for(i in (nrow(option_tree)):2){
      option_tree[i-1, 1:(2^(N-1))] <-
        (1/(1+r))*(q*option_tree[i, seq(2, ncol(option_tree), by = 2)] + (1-q)*option_tree[i, seq(1, ncol(option_tree), by = 2)])
      
      tmp1 <- option_tree[i-1, 1:(2^(N-1))]
      tmp2 <- rep(0,length(tmp1))
      
      if((i-1) >= 1){
        tmp2[1:length(stock_development(S, i-2, u, d))] <- pmax(K - stock_development(S, i-2, u, d), 0)
      }
      
      option_tree[i-1, 1:(2^(N-1))] <- pmax(tmp1, tmp2)      
      stock_row = stock_development(S, i-1, u, d)
      deltas[i-1, 1:(2^(N-1))] <- (option_tree[i, seq(2, ncol(option_tree), by = 2)] - option_tree[i, seq(1, ncol(option_tree), by = 2)])/(stock_row[seq(2, ncol(option_tree), by = 2)] - stock_row[seq(1, ncol(option_tree), by = 2)])
    }
    
    deltas[is.na(deltas)] <- 0
    
    return(list(derivative_prices = option_tree, delta = deltas))
  } 
  
  
  for(i in (nrow(option_tree)):2){
    option_tree[i-1, 1:(2^(N-1))] <-
      (1/(1+r))*(q*option_tree[i, seq(2, ncol(option_tree), by = 2)] + (1-q)*option_tree[i, seq(1, ncol(option_tree), by = 2)])
    
    stock_row = stock_development(S, i-1, u, d)
    deltas[i-1, 1:(2^(N-1))] <- (option_tree[i, seq(2, ncol(option_tree), by = 2)] - option_tree[i, seq(1, ncol(option_tree), by = 2)])/(stock_row[seq(2, ncol(option_tree), by = 2)] - stock_row[seq(1, ncol(option_tree), by = 2)])
  }
  
  deltas[is.na(deltas)] <- 0
  
  return(list(derivative_prices = option_tree, delta = deltas))
  
}


European_put1 = Derivative_Security(1, 1, 1, (-(1/8)), (3/4), (5/4), "European_put")
European_put1

European_put2 = Derivative_Security(4, 7, 4, (1/4), (1/2), 2, "European_call")
European_put2
European_put3 = Derivative_Security(4, 4, 4, (1/4), (1/2), 2, "Asian_call")
European_put3
European_put4 = Derivative_Security(4, 4, 4, (1/4), (1/2), 2, "Asian_put")
European_put4
European_put5 = Derivative_Security(4, 4, 4, (1/4), (1/2), 2, "American_put")
European_put5
European_put6 = Derivative_Security(4, 4, 4, (1/4), (1/2), 2, "European_put")
European_put6



