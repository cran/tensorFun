
#' Tensor unfolding
#'
#' @description Performing tensor unfolding, also known as matricization
#' @param ten A multi-dimensional array
#' @param k An integer specifying the mode of array to unfold
#'
#' @return A matrix
#' @export
#' @import ClimProjDiags
#'
#' @examples
#' unfold(array(1:24, dim=c(3,4,2)), 2)
#'
#'
unfold <- function(ten,k){
  if (requireNamespace('ClimProjDiags', quietly = TRUE)){
    mat <- matrix(nrow= prod(dim(ten)[-k]) , ncol= length(dim(ten))-1)
    for (i in 1:(length(dim(ten))-1)){
      if (i==1){
        a <- dim(ten)[-k][1]
        mat[,1] <- rep((1:a), nrow(mat) / a )
      }else{
        a <- dim(ten)[-k][i]
        a <- unlist(lapply((1:a), rep, times= prod(dim(ten)[-k][1:(i-1)])))
        mat[,i] <- rep(a, nrow(mat) / length(a))
      }
    }

    result <- matrix(nrow = dim(ten)[k], ncol=prod(dim(ten)[-k]))
    for (i in 1:ncol(result)){
      result[,i] <- Subset(ten, (1:length(dim(ten)))[-k], indices = as.list( mat[i,] ))
    }
    result
  }
}
