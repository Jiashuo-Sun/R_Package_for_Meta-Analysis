metaGroup8 <-
function(a,b,c = NULL, d = NULL, e = NULL, method = c('fisher','stouffer','minP','maxP')){
  
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
