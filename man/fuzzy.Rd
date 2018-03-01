\name{fuzzy}

\alias{fuzzy}
\alias{fuzzy,numeric-method}

\title{Fuzzification}

\description{
Convert a numeric vector to a set of fuzzy values given the parameters of a fuzzy membership function.
}

\usage{
fuzzy(x, params, type) 
}

\arguments{
  \item{x}{ the input numeric vector }
  \item{type}{ a character specifying the type of the fuzzy membership function. See details. }
  \item{params}{ a vector including the parameter values given the \code{type} of the fuzzy membership function }
  
  
}
\details{
The following options can be selected as the type of the fuzzy membership function in \code{type}. For each type, the required parameters should be specified in \code{params}:

- "decreasing" or "dec": a decreasing linear membership function; \code{params = c(p1,p2)}

- "increasing" or "inc": an increasing linear membership function; \code{params = c(p1,p2)}

- "bell" or "bel": a bell-shaped fuzzy membership function; \code{params = c(p1,p2,p3,p4)}

- "gaussian" or "gau": a Gaussian fuzzy membership function; \code{params = c(p1,p2)}

}

\value{
a numeric vector of the fuzzy values with the equal size of the input values \code{x}
}


\author{Babak Naimi \email{naimi.b@gmail.com}

\url{http://r-gis.net}
}


\examples{
x <- rnorm(100,10,3) # a ranomly generated numeric vector

# increasing linear:

f1 <- fuzzy(x, type= 'inc', params= c(7, 12))

plot(x,f1)

f2 <- fuzzy(x, type= 'dec', params= c(7, 12))

plot(x,f2)


f3 <- fuzzy(x, type= 'bel', params= c(6, 9,11,14))

plot(x,f3)

f4 <- fuzzy(x, type= 'gau', params= c(11,2))

plot(x,f4)


}

\keyword{fuzzy}
