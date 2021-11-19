\name{mysort}
\alias{myort}
\title{mysort}
\usage{
sort(x, decreasing = FALSE, ...)

## Default method:
mysort(x, decreasing = FALSE, na.last = NA, ...)

mysort.int(x, partial = NULL, na.last = NA, decreasing = FALSE,
         method = c("auto", "shell", "quick", "merge"), index.return = FALSE)
}
\arguments{
x:

for sort an R object with a class or a numeric, complex, character or logical vector. For sort.int, a numeric, complex, character or logical vector, or a factor.

decreasing:

logical. Should the sort be increasing or decreasing? For the "radix" method, this can be a vector of length equal to the number of arguments in .... For the other methods, it must be length one.

... :

arguments to be passed to or from methods or (for the default methods and objects without a class) to mysort.int.

na.last:

for controlling the treatment of NAs. If TRUE, missing values in the data are put last; if FALSE, they are put first; if NA, they are removed.

partial:

NULL or a vector of indices for partial sorting.

method:

character string specifying the algorithm used. Not available for partial sorting.

index.return:

logical indicating if the ordering index vector should be returned as well. Supported by all methods for any na.last mode and data type.

\description{
mysort is a generic function for which methods can be written, and mysort.int is the internal method which is compatible with S if only the first three arguments are used.

The "auto" method selects "merge" for short (less than 2^31 elements) vectors vectors and factors; otherwise, "shell".

"Merge" sort is an efficient, general-purpose, and comparison-based sorting algorithm. Most implementations produce a stable sort, which means that the order of equal elements is the same in the input and output with worst case complexity of O({n*log(n)}).

If partial is not NULL, it is taken to contain indices of elements of the result which are to be placed in their correct positions in the sorted array by partial sorting. For each of the result values in a specified position, any values smaller than that one are guaranteed to have a smaller index in the sorted array and any values which are greater are guaranteed to have a bigger index in the sorted array. (This is included for efficiency, and many of the options are not available for partial sorting. It is only substantially more efficient if partial has a handful of elements, and a full sort is done (a Quicksort if possible) if there are more than 10.) Names are discarded for partial sorting.

Method "shell" uses Shellsort (an O(n^{4/3}) variant from Sedgewick (1986)). If x has names a stable modification is used, so ties are not reordered. (This only matters if names are present.)

Method "quick" uses Singleton (1969)'s implementation of Hoare's Quicksort method and is only available when x is numeric (double or integer) and partial is NULL. (For other types of x Shellsort is used, silently.) It is normally somewhat faster than Shellsort (perhaps 50\% faster on vectors of length a million and twice as fast at a billion) but has poor performance in the rare worst case. (Peto's modification using a pseudo-random midpoint is used to make the worst case rarer.) This is not a stable sort, and ties may be reordered.
}
\examples{
mysort(c('1','2', NA,'5','8','0', NA), partial = c(4), index.return = T, na.last = NA, method = "merge")
#result:
#$x
#[1] "1" "2" "0" "5" "8"
#
#$ix
#[1] 1 2 6 4 5

#with all arguments:
mysort(c('1','2', NA,'5','8','0', NA), partial = c(4), index.return = T, na.last = NA, partial = c(2,4)) #'NAs'

#Default usage
mysort(c(3,2,4,1,5))

## A larger test
x <- rnorm(1e5)
system.time(x1 <- mysort(x, method = "shell"))
system.time(x2 <- mysort(x, method = "quick"))
system.time(x3 <- mysort(x, method = "merge"))

## End(Not run)
}
