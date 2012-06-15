
func1 <- function() {
  my.function.2("this is the OLD call")
}

func2 <- function(x) {
  print(x)
  stop("this is the OLD error message")
}
                             
