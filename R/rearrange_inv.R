
#' Inverse rearrangement algorithm on mode k
#'
#' @description The inverse algorithm of rearrangement algorithm for one mode
#' @param ten A multi-dimensional array
#' @param k An integer specifying the mode k to rearrange backwards
#' @param l A vector specifying the original indices rearranged
#'
#' @return A multi-dimensional array
#' @export
#'
rearrange_inv <- function(ten, k, l){
  if (length(l)==0){
    return(ten)
  }

  a <- unfold(ten, k)

  result <- matrix(nrow=nrow(a),ncol=ncol(a))
  result[-l,] <- a[1:(nrow(a)-length(l)),]
  result[l,] <- a[(nrow(a)-length(l)+1):nrow(a),]

  return( refold(result, k, dim(ten)) )
}
