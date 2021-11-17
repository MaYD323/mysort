\name{mysort}
\alias{myort}
\title{mysort}
\usage{
sort(x, decreasing = FALSE, ...)

## Default method:
sort(x, decreasing = FALSE, na.last = NA, ...)

sort.int(x, partial = NULL, na.last = NA, decreasing = FALSE,
         method = c("auto", "shell", "quick", "merge"), index.return = FALSE)
}
\arguments{
x:{

for sort an R object with a class or a numeric, complex, character or logical vector. For sort.int, a numeric, complex, character or logical vector, or a factor.

decreasing:

logical. Should the sort be increasing or decreasing? For the "radix" method, this can be a vector of length equal to the number of arguments in .... For the other methods, it must be length one. Not available for partial sorting.

... :

arguments to be passed to or from methods or (for the default methods and objects without a class) to sort.int.

na.last:

for controlling the treatment of NAs. If TRUE, missing values in the data are put last; if FALSE, they are put first; if NA, they are removed.

partial:

NULL or a vector of indices for partial sorting.

method:

character string specifying the algorithm used. Not available for partial sorting. Can be abbreviated.

index.return:

logical indicating if the ordering index vector should be returned as well. Supported by all methods for any na.last mode and data type.
}

\description{
Prints 'Hello, world!'.
}
\examples{
hello()
}
