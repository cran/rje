patternRepeat <-
function (x, which, n, careful=TRUE) 
{
  if (careful) {
    tmp = unique(which)
    if (!all(tmp == which)) 
      warning("repeated indices ignored")
    which = sort(tmp)
    if (prod(n[which]) != length(x)) 
      stop("x not of correct length")
    if (length(which) > length(n)) 
      stop("which too long")
  }
  
  tmp = setdiff(seq_along(n), which)
  if (length(tmp) == 0) 
    return(x)
  add.in = min(tmp)
  bl = prod(n[seq_len(add.in - 1)])
  x.new = x[rep.int(seq_len(bl), n[add.in]) + rep(seq.int(from = 0, 
                                                  by = bl, length.out = length(x)/bl), each = bl * n[add.in])]
  which.new = c(seq_len(add.in), which[which > add.in])
  
  return(Recall(x.new, which.new, n, careful=FALSE))
}
