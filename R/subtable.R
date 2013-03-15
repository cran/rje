subtable <-
function(x, variables, value, drop=TRUE){
  indexlist <- lapply(dim(x), seq_len)
  indexlist[variables] <- value
  dims = dim(x)
  dims[variables] = sapply(value, length)
  if (isTRUE(drop)) dims = dims[!(seq_along(dims) %in% variables) | dims > 1]

  out = subarray(x, indexlist)
  out = array(out, dim=dims)

  return(out)
}
