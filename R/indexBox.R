indexBox <-
function(upp, lwr, dim) {
  if (any(upp < 0) || any(lwr > 0)) stop("Incorrect bounds")
  if (any(dim <= 0)) stop("Incorrect dimensions")
  upp = rep_len(upp, length(dim))
  lwr = rep_len(lwr, length(dim))
  out = .C("indexBox", as.integer(upp), as.integer(lwr), as.integer(dim), length(dim), integer(prod(upp-lwr+1)), package="rje")
  return(out[[5]])
}
