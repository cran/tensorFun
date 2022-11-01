
#' Mode-k Block-a Block-b transformation matrix
#'
#' @description Defined for an ongoing working paper
#' @param A A matrix with n rows and l columns
#' @param B A matrix with k rows and l columns, with k no larger than n
#'
#' @return A matrix
#' @export
#' @import MASS
#'
H_trans <- function(A,B){
  if (requireNamespace('MASS', quietly = TRUE)){
    #Moore-Penrose pseudo-inverse of the cut of matrix
    inv.A <- ginv( Mcut(B, A) )
    return(inv.A%*%B)
  }
}
