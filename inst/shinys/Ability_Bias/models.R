

run <- function(alpha){
    d = makedata(alpha1 = alpha)
    ols <- coef(lm(y ~ X + s, d))
    iv  <- coef(estimatr::iv_robust(y ~ X + s | X + Z,d))
    return(list(ols_s = ols["s"],
                iv_s  = iv["s"]))
}

makedata <- function(alpha1 = 2,n = 1000){
    # y = b0 + b1 x + g1 A + b2 s + u
    # s = a0 + g1 A + a1 Z + e
    
    # true values
    # y equation
    b0 = 0.7
    b1 = 0.3
    b2 = 1.0
    
    # s equation
    a0 = 5.0
    g0 = 0.01
    g1 = 5.7
    
    set.seed(1111)
    
    d = data.table(u = rnorm(n),
                   e = rnorm(n),
                   A = rnorm(n,mean = 0.5, sd = 0.25),
                   Z = rnorm(n),
                   X = rbinom(n,size = 1,prob = 0.4))
    # impose truncations
    d[A > 1 , A := 1]
    d[A < 0 , A := 0]
    d[Z > 2 , Z := 2]
    d[Z < -2 , Z := -2]
    
    # put together data
    d[, s := a0 + g1 * A + alpha1 * Z + e]
    d[, y := b0 + b1 * X + (g1 * A) + b2 * s + u]
    d
}