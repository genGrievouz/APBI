BubbleSort <- function(x){
  n<-length(x)-1
  for(j in 1:n){
    for(i in 1:n){
      if(x[i]>x[i+1]){
        uloz<-x[i]
        x[i]<-x[i+1]
        x[i+1]<-uloz
      }
    }
  }
  return(x)
}


