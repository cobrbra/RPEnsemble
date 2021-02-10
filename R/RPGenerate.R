#' Generates random matrices
#'
#' @description Generates B2 random p by d matrices according to Haar measure, Gaussian or axis-aligned projections.
#'
#' @param p The original data dimension.
#' @param d The lower dimension.
#' @param method Projection distribution, either "Haar" for Haar distributed projections, "Gaussian" for Gaussian distributed projections with i.i.d. N(0,1/p) entries, "axis" for uniformly distributed axis aligned projections, or "other" for user defined method.
#' @param B2 The number of projections
#'
#' @return Returns B2 random matrices, each of size p by d, as a single p by d*B2 matrix.
#' @export
#'
#' @examples
#' R1 <- RPGenerate(p = 20, d = 2, "Haar", B2 = 3)
#' t(R1)%*%R1
#' R2 <- RPGenerate(p = 20, d = 2, "Gaussian", B2 = 3)
#' t(R2)%*%R2
#' R3 <- RPGenerate(p = 20, d = 2, "axis", B2 = 3)
#' colSums(R3)
#' rowSums(R3)

RPGenerate <- function(p=100, 
                       d=10,   
                       method = "Haar", 
                       B2 = 10) {
    
    if (p < d) {
        stop("d must be less than p")
    }
    
    if (method == "Gaussian") {
        Q <-matrix(1/sqrt(p)*rnorm(p*d*B2,0,1),p,d*B2)
    }   
       
    if (method == "Haar") {
        R0 <- matrix(1/sqrt(p)*rnorm(p*d*B2,0,1),p,d*B2)
        Q <- matrix(sapply(1:B2-1, FUN = function(s){qr.Q(qr(R0[,s*d+1:d]))[,1:d]}),p,B2*d)
    }           
    
    if (method == "axis") {
        Q <- NULL
        
        for(i in 1:B2) {
            S <- sample(1:p,d)
            R <- matrix(0,p,d)
            
            for (D in 1:d) {
                R[S[D],D] <- 1
            }
            
            Q <- cbind(Q,R)
        }
    }
       
    if (method == "other") {
        Q <- matrix(0,p,d)
    }
        
    return(Q)
}
