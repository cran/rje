intervention.table <-
function(x, variables, condition) {
  tmp = condition.table2(x, variables, condition)
  out = x/tmp
  out
}
