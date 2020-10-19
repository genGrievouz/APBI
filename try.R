x <- unlist(strsplit("jebanograndowlesando",''))
y <- unlist(strsplit("idiotyzm",''))
i <- 1

l_x <- length(x)
l_y <- length(y)
r <- l_x - l_y
new_y <- c()



while(i <= r){
  uloz<-y[i]
  y[i+l_y]<-uloz
  i = i + 1 
}

