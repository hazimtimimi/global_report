# Philippe's aggregation functions ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

add.rv <- function (r, r.lo, r.hi, r.sd, weights = 1, method = "beta")
{
  if (is.null(r) || length(r) == 0)
    stop("Error: r must contain at least one value")
  if (sum(r < 0 & !is.na(r) & method == "beta"))
    stop("Error: r must be positive with method 'beta'")
  if (sum(r > 1 & !is.na(r) & method == "beta"))
    stop("Error: r must be between 0 and 1 with method 'beta'")
  if (missing(r.sd))
    r.sd <- (r.hi - r.lo)/4
  if (missing(r.lo) & !missing(r.sd))
    r.lo <- numeric()
  if (missing(r.hi) & !missing(r.sd))
    r.hi <- numeric()
  if (sum(r.lo < 0 & !is.na(r.lo) & method == "beta"))
    stop("Error: r.lo must be positive with method 'beta'")
  if (sum(r.lo > 1 & !is.na(r.lo) & method == "beta"))
    stop("Error: r.lo must be between 0 and 1 with method 'beta'")
  if (sum(r.hi < 0 & !is.na(r.hi) & method == "beta"))
    stop("Error: r.hi must be positive with method 'beta'")
  if (sum(r.hi > 1 & !is.na(r.hi) & method == "beta"))
    stop("Error: r.hi must be between 0 and 1 with method 'beta'")
  if (sum(r.sd > 1 & !is.na(r.sd) & method == "beta"))
    stop("Error: sd must be between 0 and 1 with method 'beta'")
  if (sum(r[!is.na(r) & is.na(r.sd)]))
    stop("Error: some values for r are supplied without uncertainty")
  if (sum(r.sd < 0 & !is.null(r.sd) & !is.na(r.sd)))
    stop("Error: sd must be positive")
  if (!is.null(r.sd))
    v <- r.sd^2
  else v <- ((r.hi - r.lo)/4)^2
  sw <- ifelse(length(weights) > 1, sum(weights[!is.na(r)],
                                        na.rm = TRUE), 1)
  out.m <- sum(r * weights, na.rm = TRUE)/sw
  out.v <- ifelse(length(weights) > 1, sum(v[!is.na(r)] * weights[!is.na(r)]^2,
                                           na.rm = TRUE)/sw^2, sum(v))
  if (method == "beta") {
    S <- (out.m * (1 - out.m)/out.v) - 1
    a <- S * out.m
    b <- S * (1 - out.m)
    lo <- qbeta(0.025, a, b)
    hi <- qbeta(0.975, a, b)
  }
  else {
    lo <- qnorm(0.025, out.m, sqrt(out.v))
    hi <- qnorm(0.975, out.m, sqrt(out.v))
  }
  if (all(weights == 1))
    return(data.frame(r = out.m, r.lo = lo, r.hi = hi, r.sd = sqrt(out.v)))
  else return(data.frame(r = out.m, r.lo = lo, r.hi = hi, r.sd = sqrt(out.v),
                         r.num = out.m * sw, r.lo.num = lo * sw, r.hi.num = hi *
                           sw, e.pop.num = sw))
}


# product of two random variables X and Y using Taylor expansion approximation
prodXY <- function(X, Y, varX, varY, covXY=0){
  eXY <- X * Y + covXY
  varXY <- X^2*varY + Y^2*varX + 2*X*Y*covXY + varX*varY + covXY^2
  return(list("E(XY)"=eXY, "Var(XY)"=varXY))
}


# ratio of two random variables X and Y using Taylor expansion
divXY <- function(X, Y, varX, varY, covXY=0){
  eXY <- X/Y - covXY/Y^2 + X*varY/Y^3
  varXY <- varX/Y^2 - 2*X*covXY/Y^3 + X^2*varY/Y^4
  return(list("E(X/Y)"=eXY, "Var(X/Y)"=varXY))
}
