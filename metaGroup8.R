metaGroup8  = function(a,b,c = NULL, d = NULL, e = NULL, method = c('fisher','stouffer','minP','maxP')){
  
  checkresult = colcheck(a,b,c,d,e)
  if (checkresult == 1){
    all.p.value = beststatest(a)
    all.p.value = cbind(all.p.value, beststatest(b))
    all.p.value = cbind(all.p.value, beststatest(c))
    all.p.value = cbind(all.p.value, beststatest(d))
    all.p.value = cbind(all.p.value, beststatest(e))
    
    #print(t(all.p.value))
    p.value = apply(all.p.value, 1, poolp, method = method)
    
    return(list(biomarkers = names(a[-1]), pooled.p.value = p.value))
  }
  
  else{
    print("Error: Different length of colume")
    return(NULL)
  }

}

colcheck<-function(a,b,c=NULL,d=NULL,e=NULL){
  # correct is 1
  ## print("They have the same length of colume"
  # error is 0
  #print("Error: Different length of colume")
  if (!is.null(e)&&!is.null(d)&&!is.null(c)){
    if (ncol(a)==ncol(b)&&ncol(b)==ncol(c)&&ncol(b)==ncol(d)&&ncol(b)==ncol(e)){
      return(1)
    }
    else{return(0)}
  }
  if (is.null(e)&&!is.null(d)&&!is.null(c)){
    if (ncol(a)==ncol(b)&&ncol(b)==ncol(c)&&ncol(b)==ncol(d)){return(1)}
    else{return(0)}
  }
  if (is.null(e)&&is.null(d)&&!is.null(c)){
    if (ncol(a)==ncol(b)&&ncol(b)==ncol(c)){return(1)}
    else{return(0)}
  }
  if (is.null(e)&&is.null(d)&&is.null(c)){
    if (ncol(a)==ncol(b)){return(1)}
    else{return(0)}
  }
}

beststatest = function(onedataframe){
  
  ###function of pvalue for dataframe with 2 group
  my.pvalue1<-function(x){
    pv=NULL
    z=as.numeric(length(unique(x[,1])))    #z is number of group
    if (z!=2) {
      print("Error: The dataframe has more than 2 group")
    }
    if(z==2) {
      for (i in 2:dim(x)[2]) {
        nor=shapiro.test(x[,i])$p.value
        #if it is normal distribution and we do the 2 sample t.test
        if (nor>=0.05){
          a1=x[,i][x[,1]==1]
          a2=x[,i][x[,1]==2]
          a=var.test(a1,a2)$p.value
          if (a>=0.05){  #equal variance
            pvalue=t.test(a1,a2,var.equal=TRUE)$p.value}
          if (a<0.05){ #difference variance
            pvalue=t.test(a1,a2,var.equal=FALSE)$p.value}
          pv=c(pv,pvalue)
        }
        #if it is not normal distribution, we do the wilcox rank sum test
        if (nor<0.05){
          a1=x[,i][x[,1]==1]
          a2=x[,i][x[,1]==2]
          pvalue=wilcox.test(a1,a2)$p.value
          pv=c(pv,pvalue)
        }}
      return(pv)
    }}
  
  ###function of pvalue for dataframe with more than 2 group
  my.pvalue2<-function(x){
    a=as.numeric(length(unique(x[,1])))    #a is number of group
    pv=NULL
    for (i in 2:dim(x)[2]){
      A=NULL
      ALL=NULL
      group=NULL
      nor=shapiro.test(x[,i])$p.value
      #if it is normal distribution and we do the one way anova test
      if (nor>=0.05){
        for (j in 1:a){
          A[j]=as.data.frame(x[,i][x[,1]==j])
          ALL=c(ALL,as.array(A[j])[[1]])
          group=c(group,rep(j,length(as.array(A[j])[[1]])))
        }
        ydata=data.frame(ALL=ALL,group=factor(group))
        fit=lm(ALL~group,data=ydata)
        pvalue=anova(fit)$Pr[1]
        pv=c(pv,pvalue)
      }
      #if it is not normal distribution, we do the Kruskal Wallis test
      if (nor<0.05){
        #3 groups
        pvalue=kruskal.test(x[,i]~as.factor(x[,1]))$p.value
        pv=c(pv,pvalue)
      }}
    return(pv)
  }
  
  x = onedataframe
  if (is.null(x)){return(NULL)}
  a=as.numeric(length(unique(x[,1])))
  if (a==2){
    return(my.pvalue1(x))}
  if (a > 2){
    return(my.pvalue2(x))}
  
}

poolp = function(p, method = c('fisher','stouffer','minP','maxP')){
  
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
