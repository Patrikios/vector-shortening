fx <- function(x, breakPoint) {
  accm <- x[length(x)]
  i <- length(x)-1
  condition <- function(i) sum(accm, x[i], x[i-1], na.rm = T) < breakPoint
  while(condition(i)) {
    accm <- sum(accm, x[i], na.rm = T)
    i <- i - 1
  }
  return(x[i:length(x)])
}
fx2 <- function(x, breakPoint) {
  i <- length(x)
  accm <- x[i]
  i <- i - 1
  condition <- function(i) sum(accm, x[i], x[i-1], na.rm = T) < breakPoint
  while(condition(i)) {
    accm <- sum(accm, x[i], na.rm = T)
    i <- i - 1
  }
  return(x[i:length(x)])
}

vec <- c(1:1000000,3,5,3,4,3,9,1,8,2,5)

microbenchmark::microbenchmark(
  Sotos = { while (sum(vec, na.rm = TRUE) >= 20) {vec <- vec[-1]} },
  Ronak = tail(vec, sum(cumsum(replace(rev(vec), is.na(rev(vec)), 0)) < 20)),
  Wimpel = rev( rev(vec)[cumsum( replace( rev(vec), is.na( rev(vec) ), 0 ) ) < 20]),
  WimpelMarkus = vec[rev(cumsum(rev(replace(vec, is.na(vec), 0))) < 20)],
  Patrik1 = vec[Reduce(f = "+", x = vec, accumulate = T, right = T) < 20],
  Patrik2 = fx(vec, breakPoint = 20), 
  Patrik3 = fx2(vec, breakPoint = 20),
  times = 100, unit = "ms"
)

shorten <- function(x, breakPoint, fromLeft = F) {
  if(fromLeft) {
    accm <- x[1]
    i <- 2
    condition <- function(i) sum(accm, x[i], x[i+1], na.rm = T) < breakPoint
    while(condition(i)) {
      accm <- sum(accm, x[i], na.rm = T)
      i <- i + 1
    }
    return(x[1:i])
  } else {
    accm <- x[length(x)]
    i <- length(x)-1
    condition <- function(i) sum(accm, x[i], x[i-1], na.rm = T) < breakPoint
    while(condition(i)) {
      accm <- sum(accm, x[i], na.rm = T)
      i <- i - 1
    }
    return(x[i:length(x)])
  }
}

shorten <- function(x, breakPoint) {
  accm <- x[length(x)]
  i <- length(x)-1
  condition <- function(i) sum(accm, x[i], x[i-1], na.rm = T) < breakPoint
  while(condition(i)) {
    accm <- sum(accm, x[i], na.rm = T)
    i <- i - 1
  }
  return(x[i:length(x)])
}

vec = c(rep(c(seq(1, 100, 5), NA), 10000), 3, 2, NA, 4, 5, 1, 2, 3, 4, 9, NA, 1, 2)
shorten(vec, 1000)

microbenchmark::microbenchmark(
  Sotos = { while (sum(vec, na.rm = TRUE) >= 1000) {vec <- vec[-1]} },
  Ronak = tail(vec, sum(cumsum(replace(rev(vec), is.na(rev(vec)), 0)) < 1000)),
  Wimpel = rev( rev(vec)[cumsum( replace( rev(vec), is.na( rev(vec) ), 0 ) ) < 1000]),
  WimpelMarkus = vec[rev(cumsum(rev(replace(vec, is.na(vec), 0))) < 1000)],
  shorten(vec, 1000),
  times = 1000, unit = "ms"
)

cppFunction('
  IntegerVector shorthen_cpp_(IntegerVector &vec, int &breakPoint, bool &fromRight) {
              if(fromRight){
                int L = vec.size()-1;
                int i = L;
                int accm = vec[i];
                --i;
                int C = accm + vec[i] + vec[i-1];
                while(C<breakPoint) {
                    accm = accm + vec[i];
                    --i;
                    C = accm + vec[i] + vec[i-1]; 
                }
                return vec[Range(i, L)];
              } else {
                int i = 0;
                int accm = vec[i];
                ++i;
                int C = accm + vec[i] + vec[i+1];
                while(C<breakPoint) {
                    accm = accm + vec[i];
                    ++i;
                    C = accm + vec[i] + vec[i+1]; 
                }
                return vec[Range(0, i)];
              }
}')

vec <- c(1:19999, 3,5,3,4,3,9,1,8,2,5)
shorthen_cpp_(vec, breakPoint = 20, fromRight = T)



microbenchmark::microbenchmark(
  Sotos = { while (sum(vec, na.rm = TRUE) >= 20) {vec <- vec[-1]} },
  Ronak = tail(vec, sum(cumsum(replace(rev(vec), is.na(rev(vec)), 0)) < 20)),
  Wimpel = rev( rev(vec)[cumsum( replace( rev(vec), is.na( rev(vec) ), 0 ) ) < 20]),
  WimpelMarkus = vec[rev(cumsum(rev(replace(vec, is.na(vec), 0))) < 20)],
  Patrik = shorthen_cpp_(vec, breakPoint = 20, fromRight = T),
  times = 1000, unit = "ms"
)
