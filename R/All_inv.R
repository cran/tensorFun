
#' Inverse rearrangement algorithm on all modes
#'
#' @description The inverse algorithm of rearrangement algorithm for all modes
#' @param ten_bundle A list of two objects, a multi-dimensional array and a list of indices along the modes to reverse
#' @param mode A vector implying the modes to reverse, correspondent to the list in 'ten_bundle', set as 'all' by default
#'
#' @return A multi-dimensional array
#' @export
#'
All_inv <- function(ten_bundle, mode='all'){
  a <- ten_bundle[[1]]
  b <- ten_bundle[[2]]

  if (length(mode)==1){
    if (mode == 'all'){
      mode <- 1:length(dim(a))
    }
  }

  if (length(mode) != length(b)){
    message('The length of modes and indices is inconsistent!')
    return()
  }

  for (i in mode){
    a <- rearrange_inv(a, i, b[[i]])
  }

  return(a)
}
