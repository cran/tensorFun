
#' Tensor refolding
#'
#' @description Performing tensorization, which is the inverse process of unfolding
#' @param unfolding A multi-dimensional array
#' @param k An integer specifying the mode of array to unfold
#' @param dim_vec A vector specifying the expected dimension of output array
#'
#' @return A multi-dimensional array
#' @export
#' @import psychTools
#'
#' @examples
#' refold(matrix(1:9,nrow=3), 1, c(3,1,3))
#'
refold <- function(unfolding, k, dim_vec){
  if (requireNamespace('psychTools', quietly = TRUE)){
    mat <- matrix(nrow= prod(dim_vec[-k]) , ncol= length(dim_vec)-1)
    for (i in 1:(length(dim_vec)-1)){
      if (i==1){
        a <- dim_vec[-k][1]
        mat[,1] <- rep((1:a), nrow(mat) / a )
      }else{
        a <- dim_vec[-k][i]
        a <- unlist(lapply((1:a), rep, times= prod(dim_vec[-k][1:(i-1)])))
        mat[,i] <- rep(a, nrow(mat) / length(a))
      }
    }

    result <- matrix(ncol= ncol(mat))
    for (i in 1:nrow(mat)){
      result <- rbind(result, matrix(mat[i,], byrow=TRUE, nrow=dim_vec[k], ncol=ncol(result)))
    }
    result <- result[-1,]

    if (k==1){
      result <- cbind(rep((1: dim_vec[k]), nrow(result)/dim_vec[k] ),result)
    }else if (k > ncol(result)){
      result <- cbind(result, rep((1: dim_vec[k]), nrow(result)/dim_vec[k] ))
    }else{
      result <- cbind(result[,1:(k-1)],
                      rep((1: dim_vec[k]), nrow(result)/dim_vec[k] ),
                      result[, k: ncol(result)])
    }

    result <- cbind(c(unfolding), result)
    result <- dfOrder(result, columns= ncol(result):2)

    final <- array(result[,1], dim=dim_vec)
    final
  }
}
