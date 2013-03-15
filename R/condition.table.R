condition.table <-
function(x, variables, condition = NULL, condition.value = NULL) {
  if (!is.null(condition.value) && 
        length(condition) != length(condition.value)) stop("condition and condition.value must have same length")
  if(length(intersect(variables, condition)) > 0) stop("margin and condition must be disjoint")

  k = length(variables)
                                                                                     
  marg = margin.table(x, c(variables, condition))
  if (length(condition) == 0) return(marg/sum(marg))

  variables <- seq_len(k)
  condition <- k + seq_along(condition)
  cond <- prop.table(marg, condition)

  if (is.null(condition.value)) {
    out = array(c(cond), dim=dim(x)[c(variables, condition)])
  }
  else if (is.list(condition.value)) {
    out = subtable(cond, condition, condition.value)
    out = array(c(out), dim=c(dim(x)[variables], sapply(condition.value, length)))
  }
  else {
    out = subtable(cond, condition, condition.value)
    out = array(c(out), dim=dim(x)[variables])
  }

  return(out)
}
