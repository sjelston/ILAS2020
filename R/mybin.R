#' Binomial simulation function
#'
#' This function creates a bar plot of the proportions of successes in a binomial experiment
#' over the given number of iterations with the sample size and probability provided.
#'
#' @param iter Number of iterations to be performed
#' @param n Number of trials in each iteration
#' @param p Probability of success
#'
#' @return A barplot of the proportions of sucesses
#' @export
#'
#' @examples
#' mybin(iter=1000,n=18, p=0.3)
mybin=function(iter=100,n=10, p=0.5){

  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  succ=c()
  for( i in 1:iter){
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    succ[i]=sum(sam.mat[,i])
  }
  succ.tab=table(factor(succ,levels=0:n))
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
