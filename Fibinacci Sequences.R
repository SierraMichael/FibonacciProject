#Fibonacci by calculations

fib.calculation <- function(h){
h1= h
h2= h+1
h3= h2 + h1
h4= h3 + h2
h5= h4 + h3
h6= h5 + h4
h7= h6 + h5
h8= h7 + h6
h9= h8 +h7
h10= h9 + h8

return(list(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10))
}
fib.calculation(0)


#Dynamic programming
fib.dynamic <- function(n){
  if(n<0){
    return("No negative input")
  } else if( n <=1){
    return(n)
    } else {
      xn=fib.dynamic(n-1) + fib.dynamic(n-2)
        return(xn)
  }
}

fib.dynamic(0)

#Sequence of Fibonacci 
fib.apply <- function(t){
  sapply(t, fib.dynamic)
}

fib.apply(1:10)

#Memoization 
fib.memoization <-local({
  memorize <- list() 
  function(k) {
   remember.value <- as.character(k)
  if(k < 0){
    return("No negative input")
  }
  if(k <=1){
    return(k)
  }
  mem <- Recall(as.numeric(k-1)) + Recall(as.numeric(k-2))
  memorize[[remember.value]] <<- mem
  return(mem)
  }
})



fib.apply.m <- function(l){
  sapply(l, fib.memoization)
}

typeof(fib.apply.m)
fib.apply.m(1:10)

#Comparing functions
system.time(fib.calculation(0))
system.time(fib.apply(1:10))
system(as.numeric(fib.apply.m(1:10)))










