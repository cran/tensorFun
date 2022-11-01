
#' Rearrangement algorithm on mode k
#'
#' @description Rearrangement algorithm on one mode
#' @details A rearrangement algorithm on higher order tensor to rearrange all missing entries along mode k to the end.
#' @param ten A multi-dimensional array
#' @param k An integer specifying the mode to arrange
#' @param key The value to which rearrange is according, set as 'NA' by default
#'
#' @return A list of two objects, a rearranged tensor and the indices rearranged
#' @export
#'
rearrange <- function(ten, k, key='NA'){
  unfold.mat <- unfold(ten, k)
  l <- integer()

  row_num <- nrow(unfold.mat)
  for (i in 1:row_num){
    if (key=='NA'){
      if ( any( is.na(unfold.mat[i,])) ){
        unfold.mat <- rbind(unfold.mat, unfold.mat[i,])
        l <- c(l, i)
      }
    }else{
      if ( key %in% (unfold.mat[i,]) ){
        unfold.mat <- rbind(unfold.mat, unfold.mat[i,])
        l <- c(l, i)
      }
    }

  }

  if (length(l) != 0){
    unfold.mat <- unfold.mat[-l,]
  }
  return( list(ten = refold(unfold.mat, k, dim(ten)),
               l = l))
}

