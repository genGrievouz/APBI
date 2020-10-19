Fibonacci <- function(x){
  x = x-2 #X musi byt rovno delce celeho vektoru, na zacatku mam alrady dve cisla
  b = c(0,1)
  k <- 1
  while(k <= x){
    b = append(b,sum(b[k]+b[k+1]))
    k = k + 1
  }
  return(b)
}