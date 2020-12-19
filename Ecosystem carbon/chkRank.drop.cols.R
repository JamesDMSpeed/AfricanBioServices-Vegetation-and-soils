#Source: https://github.com/lme4/lme4/blob/master/R/modular.R

##' Coefficients (columns) are dropped from a design matrix to
##' ensure that it has full rank.
##'
##'  Redundant columns of the design matrix are identified with the
##'  LINPACK implementation of the \code{\link{qr}} decomposition and
##'  removed. The returned design matrix will have \code{qr(X)$rank}
##'  columns.
##'
##' (Note: I have lifted this function from the ordinal (soon Rufus)
##' package and modified it slightly./rhbc)
##'
##' @title Ensure full rank design matrix
##' @param X a design matrix, e.g., the result of
##' \code{\link{model.matrix}} possibly of less than full column rank,
##' i.e., with redundant parameters.
##' @param silent should a message not be issued if X is column rank
##' deficient?
##' @return The design matrix \code{X} without redundant columns.
##' @seealso \code{\link{qr}} and \code{\link{lm}}
##' @importFrom Matrix rankMatrix
##' @author Rune Haubo Bojesen Christensen (drop.coef()); Martin Maechler
chkRank.drop.cols <- function(X, kind, tol = 1e-7, method = "qr.R") {
  ## Test and match arguments:
  stopifnot(is.matrix(X))
  kinds <- eval(formals(lmerControl)[["check.rankX"]])
  if (!kind %in% kinds) stop(gettextf("undefined option for 'kind': %s", kind))
  ## c("message+drop.cols", "ignore",
  ##   "silent.drop.cols", "warn+drop.cols", "stop.deficient"),
  
  if(kind == "ignore") return(X)
  ## else :
  p <- ncol(X)
  if (kind == "stop.deficient") {
    if ((rX <- rankMatrix(X, tol=tol, method=method)) < p)
      stop(gettextf(sub("\n +", "\n",
                        "the fixed-effects model matrix is column rank deficient (rank(X) = %d < %d = p);
                   the fixed effects will be jointly unidentifiable"),
                    rX, p), call. = FALSE)
  } else {
    ## kind is one of "message+drop.cols", "silent.drop.cols", "warn+drop.cols"
    ## --> consider to drop extraneous columns: "drop.cols":
    
    ## Perform the qr-decomposition of X using LINPACK method,
    ## as we need the "good" pivots (and the same as lm()):
    ## FIXME: strongly prefer rankMatrix(X, method= "qr.R")
    qr.X <- qr(X, tol = tol, LAPACK = FALSE)
    rnkX <- qr.X$rank
    if (rnkX == p)
      return(X) ## return X if X has full column rank
    ## else:
    
    ## message about no. dropped columns:
    msg <- sprintf(ngettext(p - rnkX,
                            "fixed-effect model matrix is rank deficient so dropping %d column / coefficient",
                            "fixed-effect model matrix is rank deficient so dropping %d columns / coefficients"),
                   p - rnkX)
    if (kind != "silent.drop.cols")
      (if(kind == "warn+drop.cols") warning else message)(msg, domain = NA)
    ## Save properties of X
    contr <- attr(X, "contrasts")
    asgn <- attr(X, "assign")
    
    ## Return the columns correponding to the first qr.x$rank pivot
    ## elements of X:
    keep <- qr.X$pivot[seq_len(rnkX)]
    dropped.names <- colnames(X[,-keep,drop=FALSE])
    X <- X[, keep, drop = FALSE]
    if (rankMatrix(X, tol=tol, method=method) < ncol(X))
      stop(gettextf("Dropping columns failed to produce full column rank design matrix"),
           call. = FALSE)
    
    ## Re-assign relevant attributes:
    if(!is.null(contr)) attr(X, "contrasts") <- contr
    if(!is.null(asgn))  attr(X, "assign")    <- asgn[keep]
    attr(X, "msgRankdrop") <- msg
    attr(X, "col.dropped") <- setNames(qr.X$pivot[(rnkX+1L):p],
                                       dropped.names)
  }
  X
}

