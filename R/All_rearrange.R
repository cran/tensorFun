
#' Rearrangement algorithm on all modes
#'
#' @description Rearrangement algorithm for all modes
#' @details A rearrangement algorithm on higher order tensor to rearrange all missing entries to a corner block. The case of mode-2 tensor returns to the work of Bai and Ng in 2021.
#' @param ten A multi-dimensional array
#' @param except A vector of integers implying the modes not to rearrange, set as 'NA' by default
#' @param key The value to which rearrange is according, set as 'NA' by default
#'
#' @return A list of two objects, a rearranged tensor and a list of indices rearranged (ordered by the rearranged modes)
#' @export
#'
All_rearrange <- function(ten, except='NA', key='NA'){
  K <- 1:length(dim(ten))
  if (length(except) == 1){
    if (except != 'NA'){
      K <- K[-except]
    }
  }else{
    K <- K[-except]
  }



  a <- ten
  L <- list()
  for (k in K){
    a <- rearrange(a, k)
    L <- append(L, list(a$l))
    a <- a$ten
  }

  return( list(tensor = a,
               ind = L))
}

