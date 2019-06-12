poolp <-
function(p, method = c('fisher','stouffer','minP','maxP')){
  
  fisher = function(p){
    k = length(p)
    X2 = -2 * sum(log(p))
    p.value = pchisq(X2, df = 2*k, lower.tail = F)
    return(p.value)
  }
  
  stouffer = function(p){
    k = length(p)
    T.stouffer = sum(qnorm(p)/sqrt(k))
    p.value = pnorm(T.stouffer)
    return(p.value)
  }
  
  minP = function(p){
    k <-length(p)
    min.p<-min(p)
    p.value<-pbeta(min.p,1,k)
    return(p.value)
  }
  
  maxP = function(p){
    k <- length(p)
    max.p<-max(p)
    p.value<-pbeta(max.p,k,1)
    return(p.value)
  }
  
  p = matrix(p,nrow = 1)
  p.value = apply(p, 1, method)
  
  return(p.value)
}
