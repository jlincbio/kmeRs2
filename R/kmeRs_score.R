#' @title Sort a k-mer Similarity Matrix
#'
#' @description
#' The \code{kmeRs_score} function sums the partial scores and sort the data.frame
#' to indicate the most 'different' k-mers
#'
#' @aliases kmeRs_score
#'
#' @param kmeRs_similarity_matrix the similarity matrix calculated by \code{kmeRs_similarity_matrix} function
#'
#' @return sorted similarity matrix with global.score column added; is returned as a data.frame
#'
#' @examples
#' # Calculate the example BLOSUM62 matrix and score the result
#'
#' example <- kmeRs_similarity_matrix(kmers_given = c("A", "T", "C", "G"), submat = "BLOSUM62")
#' kmeRs_score(example)
#'
#' @export

kmeRs_score <- function(x, decreasing = FALSE) {
	Sum <- apply(x, 1, sum, na.rm = TRUE)
	x <- cbind(kmeRs_similarity_matrix, Sum)
	x <- x[order(x$Sum, decreasing = decreasing),]
	return(x)
}