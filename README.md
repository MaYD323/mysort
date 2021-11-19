# yindisort
This yindisort package is HW4 for Biostats 625 from Yindi Ma(yindim), contains help page, vignettes, and source code.

This package is to re-implement the base function sort() in R, but I did extend the functionality of sorting.

Comparing with sort(), mysort() enables users to determine the ascending/descending order when using partial sort, and also can return indices during partial sort
and when na.last is not NA(can have indices with NA values) which can better help users to use results of mysort() to subscript original data.

## mysort
default function for user to sort a vector or factor (partially) into ascending or descending order.

with no extra arguments needed.

will call mysort.int if extra arguments were passed in.

## mysort.int 
internal function that take arguments
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

character string specifying the algorithm used. Not available for partial sorting.

index.return:

logical indicating if the ordering index vector should be returned as well. Supported by all methods for any na.last mode and data type.
