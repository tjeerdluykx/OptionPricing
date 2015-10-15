# Generate a binomial lattice
# for a given up, down, start value and number of steps
genlattice <- function(X0=100, u=1.1, d=.75, N=5) {
        X <- c()
        X[1] <- X0
        count <- 2
        
        for (i in 1:N) {
                for (j in 0:i) {
                        X[count] <- X0 * u^j * d^(i-j)
                        count <- count + 1
                }
        }
        return(X)
}