funkce <- function(a,b,c)
  D = b^2 -4*a*c
  if (D>0){
    x1 = (-b - sqrt(D))/(2*a)
    x2 = (-b + sqrt(D))/(2*a)
    print(x1,x2)
    return(x1,x2)
  }else if (D==0){
    x = -b/(2*a)
    print(x)
    return(x) 
  }else if (D<0){
    x1 = (-b - sqrt(as.complex(-D)))/(2*a)
    x2 = (-b + sqrt(as.complex(-D)))/(2*a)
    print(x1,x2)
    return(x1,x2)
  }
  
