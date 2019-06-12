beststatest <-
function(onedataframe){
  
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
