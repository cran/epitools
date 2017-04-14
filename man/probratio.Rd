\name{probratio}
\alias{probratio}

\title{Obtain unbiased probability ratios from logistic regression models}

\description{
  Estimates probability (prevalence or risk) ratios from logistic regression models using either 
  maximum likelihood or marginal standardization.
  When using the latter, standard errors are calculated using the delta method or bootstrap.
}

\usage{
probratio(object, parm, subset, method=c('ML', 'delta', 'bootstrap'), 
  scale=c('linear', 'log'), level=0.95, seed, NREPS=100, ...)
}


\arguments{
\item{object}{ a glm object with the family attribute equal to "binomial" }
\item{parm}{ a specification of which parameters are to be sequentially assigned predicted responses, either a vector of numbers or a vector of names. If missing, all parameters are considered except the intercept which should not be used except when the method argument is "model".}
\item{subset}{ a logical vector referring to which observations are included in the numerators and denominators of risk calculation. The default is TRUE, corresponding to a total population prediction ratios. User can supply subsets to calculate exposed population prediction ratios.}
\item{method}{ One of three ways that standard errors of prediction ratios are calculate. Maximum likelihood uses relative risk regression directly. Delta-method uses asymptotically correct normal approximations to prediction ratios. }
\item{scale}{ The scale on which marginal standardization calculates normal approximations to variability. When using ML, the log scale is the efficient parameterization.}
\item{level}{ The confidence level for confidence intervals.}
\item{seed}{ The random number generation seed}
\item{NREPS}{ The number of bootstrap samples to be drawn}
\item{...}{ Further arguments to glm when using maximum likelihood}
}

\details{
  Estimates prevalence and risk ratios from logistic regression models using either maximum likelihood or marginal standardization.
  Maximum likelihood is relative risk regression: a GLM with binomial variance structure and a log link.
  Marginal standardization averages predicted probabilities from logistic regression models in the total sample or exposed sample to obtain prevalence or risk ratios.
  Standard errors for marginal standardization estimates are calculated with the delta method or the normal bootstrap, which is not bias corrected.
  Ratios can be estimated on the linear or log scale, which may lead to different inference due to the invariance of Wald statistics.
}


\value{
  An array of ratios or log ratios, their standard errors, a z-score for a hypothesis test for the log ratio being different from 0 or the ratio being different from 1, the corresponding p-value, and the confidence interval for the estimate.
}

\references{
Muller, Clemma J., and Richard F. MacLehose. "Estimating predicted probabilities from logistic regression: different methods correspond to different target populations." International journal of epidemiology 43.3 (2014): 962-970.

Lumley, Thomas, Richard Kronmal, and Shuangge Ma. "Relative risk regression in medical research: models, contrasts, estimators, and algorithms." (2006).
} 

\author{Adam Omidpanah, \email{adam.omidpanah@wsu.edu}}

\note{Maximum likelihood estimation via Newton Raphson may result in predicted probabilities greater than 1. This dominates estimating functions and leads to either false convergence or failure. Users should attempt to refit such models themselves using glms with the family argument binomial(link=log). By modifying inputs to glm.control, domination may be averted. An ideal first step is supplying starting coefficients. Input start=c(-log(p), 0,0,...,0) where p is the prevalence of the outcome.
The current implementation of bootstrap standard errors, inference, and confidence intervals are not bias corrected. This will be updated in a later version.
}

\seealso{
  \code{\link{glm}}, \code{\link{deriv}},w
  \code{\link{predict.glm}}, \code{\link{family}}
}

\examples{
  set.seed(123)
  x <- rnorm(500)
  y <- rbinom(500, 1, exp(-1 + .3*x))
  logreg <- glm(y ~ x, family=binomial)
  confint.default(logreg) ## 95\% CI over-estimates the 0.3 log-RR
  pr1 <- probratio(logreg, method='ML', scale='log', start=c(log(mean(y)), 0)) 
  
  ## generally more efficient to calculate log-RR then exponentiate for non-symmetric 95\% CI
  pr1 <- probratio(logreg, scale='log', method='delta')
  pr2 <- probratio(logreg, scale='linear', method='delta')
  exp(pr1[, 5:6])
  pr2[, 5:6]
}

\keyword{models}
\keyword{risk}

%- \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
