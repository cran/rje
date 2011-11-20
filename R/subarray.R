subarray <-
function(x, value, drop=TRUE){
  if(length(value) != length(dim(x))){
    stop("Array and indexlist are not compatible!")
  }
  args <- c(quote(x), value, list(drop=drop))
  return(do.call("[", args))
}

