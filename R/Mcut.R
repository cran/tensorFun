
#' Matrix cutting
#'
#' @description Cutting the bottom few rows of a matrix B so that B has the same number of rows as matrix A
#' @param A A matrix with n rows and l columns
#' @param B A matrix with k rows and l columns, with k no less than n
#'
#' @return A matrix with n rows and l columns
#' @export
#'
#' @examples
#' Mcut(matrix(1:4,nrow=2), matrix(1:6,nrow=3))
#'
Mcut <- function(A,B){
  if (ncol(A)!=ncol(B)){
    message('Number of columns of two matrices do not match!')
    return()
  }else if (nrow(B)<nrow(A)){
    message('Number of Rows of the second matrix is wrong!')
    return()
  }
  return(B[1:nrow(A),])
}
