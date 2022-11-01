
#' Largest index observed
#'
#' @description Finding the largest index I along a tensor mode k such that the block until index I contains no NA
#' @param ten A multi-dimensional array
#' @param k An integer specifying the tensor mode to check
#'
#' @return An integer specifying the largest possible index
#' @export
#'
#'
obs_ind <- function(ten, k){
  #ten: tensor data as array
  #k: mode k

  a <- unfold(ten, k)
  for (i in 1:nrow(a)){
    if (sum(is.na(a[i,])) > 0){
      return (i-1)
    }
  }
  return(nrow(a))
}

