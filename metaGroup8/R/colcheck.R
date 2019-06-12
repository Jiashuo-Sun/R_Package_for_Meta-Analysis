colcheck <-
function(a,b,c=NULL,d=NULL,e=NULL){
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
