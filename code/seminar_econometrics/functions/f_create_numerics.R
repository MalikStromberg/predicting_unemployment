# this function creates robust numerics out of factors

create_numerics <- function(x) {
  ifelse(is.factor(x), return(as.numeric(levels(x))[x]), return(x))
}