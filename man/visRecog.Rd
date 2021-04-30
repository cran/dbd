\name{visRecog}
\alias{visRecog}
\docType{data}
\title{
    Visual recognition data.
}
\description{
    Counts of successes in visual recognition memory for large and
    small binary pictures.
}
\usage{data("visRecog")}
\format{
  A data frame with 30 observations on the following 4 variables.
  \describe{
    \item{\code{deck}}{An integer vector indicating which of two
          decks of cards, bearing graphic images, was used in the
          given experiment.}
    \item{\code{subject}}{An integer vector indexing the (human)
          subjects in the experiments.}
    \item{\code{tot5}}{An integer vector whose entries are counts
          of successes when the cards used consist of a
          \eqn{5 \times 5}{5 x 5} grid of \dQuote{facets}.}
    \item{\code{tot10}}{An integer vector whose entries are counts
          of successes when the cards used consist of a
          \eqn{10 \times 10}{10 x 10} grid of \dQuote{facets}.
  }
}
}
\details{Adult subjects were shown a series of cards, each bearing a
simple graphic image.  Each image resembled one face of a Rubik's
cube, formed of either a 5x5 or a 10x10 grid of facets, each facet
being either black or white.  Later, each subject was shown a
series of 20 similar cards, exactly 10 of which had been shown to
the subject previously.  The subject's task was to identify each
image as a new one, or as a previously seen one.  The response
variable \code{tot5} is the number of correct identifications,
out of 20, for the \eqn{5 \times 5}{5 x 5} cards.  Similarly the
variable \code{tot10} is the number of correct identifications for
the \eqn{10 \times 10}{10 x 10} cards.

Subjects 21--30 were (deliberately) tested with a different set of
cards than subjects 1--20, to ensure that results were not a function
of the original deck of cards.  (This seems to have no actual relevance.)
}
\source{
The data are taken from the paper sited in \bold{References} below.
They were provided by a generous email correspondent who prefers
to remain anonymous.
}
\references{Green, D. M., and Purohit, A. K. (1976).  Visual
recognition memory for large and small binary pictures. 
\emph{Journal of Experimental Psycholology: Human Learning and
Memory} \bold{2}, pp. 32--37.
}
\examples{
dbfit5  <- with(visRecog,mleDb(tot5,20,TRUE))
dbfit10 <- with(visRecog,mleDb(tot10,20,TRUE))
set.seed(42) # To get repeatable Monte Carlo p-values.
print(gof(dbfit5,obsd=visRecog[["tot5"]],MC=TRUE)$pval)   # 0.86
print(gof(dbfit10,obsd=visRecog[["tot10"]],MC=TRUE)$pval) # 0.68
}
\keyword{datasets}
