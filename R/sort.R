mysort <- function(x, decreasing = FALSE, na.last = NA, partial = NULL, method = NA, index.return = NA) {
  if(sum(!is.na(c(method, index.return)))!=0 || length(partial)!=0){
    if(is.na(index.return)){
      index.return<-FALSE #default value of index.return
    }
    #call internal function to handle extra arguments
    return(mysort.int(x, decreasing = decreasing, na.last = na.last, partial = partial, method = method, index.return = index.return))
  }
  y<-na.omit(x)
  if(is.na(na.last)){
    return(myquicksort(y, decreasing)) #default of quick sort to accelerate the speed of sort
  }
  else{
    if(na.last){
      return(c(myquicksort(y, decreasing), rep(NA, sum(is.na(x))))) #put NA at end
    }
    else{
      return(c(rep(NA, sum(is.na(x))),myquicksort(y, decreasing))) #put NA at start
    }
  }
}

mysort.int <- function(x, partial = NULL, na.last = NA, decreasing = FALSE,
                       method = "auto", index.return = FALSE){

  idx_all <- c(1:length(x)) #get indexes
  NA_index <- idx_all[is.na(x)] #get indexes of NAs
  non_NA_index <- idx_all[!is.na(x)] #get indexes of values
  y<-na.omit(x)

  if(length(partial)!=0){
    if(length(partial)<=10){ #If partial vector is too big, then use original sort
      partial_value <- x[partial]
      if(sum(is.na(partial_value))>0){
        stop("partial value has NA") #Error when some of partial vales are NA
      }
      if(index.return){ #return partial sorted with index
        sorted_part <- mypartial_sort(y,  decreasing = decreasing, partial_value = partial_value, idx=non_NA_index)
        if(is.na(na.last)){
          return(sorted_part)
        }
        if(na.last){ #put na at first
          return(list('x'=c(sorted_part$x, rep(NA, length(NA_index))), 'ix' = c(sorted_part$ix,NA_index)))
        }
        else{#put na at last
          return(list('x'=c(rep(NA, length(NA_index)), sorted_part$x), 'ix' = c(NA_index, sorted_part$ix)))
        }
      }
      else{ #return partial sorted without index
        sorted_part <- mypartial_sort(y,  decreasing = decreasing, partial_value = partial_value, idx=non_NA_index)$x
        if(is.na(na.last)){
          return(sorted_part)
        }
        if(na.last){
          return(c(sorted_part, rep(NA, length(NA_index))))
        }
        else{
          return(c(rep(NA, length(NA_index)), sorted_part))
        }
      }
    }
  }

  if(is.na(method) || method == "auto"){ #Automatically select sorting function
    sorted_part_with_idx <- myautosort(y,  decreasing = decreasing, idx=non_NA_index)
  }
  else if(method == "shell"){ #use shell sort function
    sorted_part_with_idx <- myshellsort(y,  decreasing = decreasing, idx=non_NA_index)
  }
  else if(method == "merge"){ #use merge sort function
    sorted_part_with_idx <- mymergesort(y,  decreasing = decreasing, idx=non_NA_index)
  }
  else if(method == "quick"){ #use quick sort function
    sorted_part_with_idx <- myquicksort_idx(y,  decreasing = decreasing, idx=non_NA_index)
  }
  else{ #throw error of invalid sort function
    stop("method has to be in c(\"auto\", \"shell\", \"quick\", \"merge\") ")
  }
  if(index.return){
    if(is.na(na.last)){
      return(sorted_part_with_idx)
    }
    if(na.last){
      return(list('x'=c(sorted_part_with_idx$x, rep(NA, length(NA_index))), 'ix' = c(sorted_part_with_idx$ix,NA_index)))
    }
    else{
      return(list('x'=c(rep(NA, length(NA_index)), sorted_part_with_idx$x), 'ix' = c(NA_index, sorted_part_with_idx$ix)))
    }
  }
  else{
    sorted_part <- sorted_part_with_idx$x
    if(is.na(na.last)){
      return(sorted_part)
    }
    if(na.last){
      return(c(sorted_part, rep(NA, length(NA_index))))
    }
    else{
      return(c(rep(NA, length(NA_index)), sorted_part))
    }
  }
}

myquicksort <- function(x, decreasing){
  if(length(x)<=1){
    return(x) #base case
  }
  pivot <- x[1] #pick pivot for quick sort
  non_pivot <- x[-1] #remove the pivot
  pivot_lt <- myquicksort(non_pivot[non_pivot<pivot], decreasing) #sort larger than pivot part of vector recursively
  pivot_ge <- myquicksort(non_pivot[non_pivot>=pivot], decreasing) #sort smaller than pivot part of vector recursively
  if(decreasing){
    return(c(pivot_ge, pivot, pivot_lt)) #descending order
  }
  else{
    return(c(pivot_lt, pivot, pivot_ge)) #ascending order
  }
}

mypartial_sort <- function(x, decreasing=FALSE, partial_value, idx){

  if(length(partial_value)==0){
    return(list('x'=x,'ix'=idx))
  }
  partial_value <- myquicksort(partial_value, decreasing) #order the pivots
  pivot <- partial_value[1]
  if(decreasing){ #put larger value in before
    recur_part <- mypartial_sort(x[x<pivot], decreasing, partial_value[-1], idx[x<pivot])
    sorted_v <- c(x[x>pivot], rep(pivot, sum(x==pivot)), recur_part$x)
    sorted_idx <- c(idx[x>pivot], idx[(x==pivot)], recur_part$ix)
    return(list('x'=sorted_v, "ix"=sorted_idx))
  }
  else{ #put smaller value in before
    recur_part <- mypartial_sort(x[x>pivot], decreasing, partial_value[-1], idx[x>pivot])
    sorted_v <- c(x[x<pivot], rep(pivot, sum(x==pivot)), recur_part$x)
    sorted_idx <- c(idx[x<pivot], idx[(x==pivot)], recur_part$ix)
    return(list('x'=sorted_v, "ix"=sorted_idx))
  }
}


mymergesort <- function(x,  decreasing = FALSE, idx){
  l <- length(x)
  if(l<=1){
    return(list("x"=x, "ix"=idx))
  }
  else{ #divide the vector by half and sort them seperately and merge by comparison
    half <- ceiling(l/2)
    a <- mymergesort(x[1:half], decreasing, idx[1:half])
    b <- mymergesort(x[(half+1):l], decreasing, idx[(half+1):l])
    if(!decreasing){
      ax <- c(a$x, Inf)
      aidx <- c(a$ix,Inf)
      bx <- c(b$x, Inf)
      bidx <- c(b$ix,Inf)
      for(el in 1:l){
        if((ax[1]>=bx[1])){
          x[el] = bx[1]
          idx[el] = bidx[1]
          bx = bx[-1]
          bidx = bidx[-1]
        }
        else{
          x[el] = ax[1]
          idx[el] = aidx[1]
          ax = ax[-1]
          aidx = aidx[-1]
        }
      }
    }
    else{
      ax <- c(a$x, -Inf)
      aidx <- c(a$ix,-Inf)
      bx <- c(b$x, -Inf)
      bidx <- c(b$ix,-Inf)
      for(el in 1:l){
        if((ax[1]<=bx[1])){
          x[el] = bx[1]
          idx[el] = bidx[1]
          bx = bx[-1]
          bidx = bidx[-1]
        }
        else{
          x[el] = ax[1]
          idx[el] = aidx[1]
          ax = ax[-1]
          aidx = aidx[-1]
        }
      }
    }
    return(list('x'=x, "ix"=idx))
  }
}

myquicksort_idx <- function(x, decreasing = FALSE, idx){ #same as the defualt quick sort but also sorted indexes
  if(length(x)<=1){
    return(list('x'=x, 'ix'=idx))
  }
  pivot <- x[1]
  non_pivot <- x[-1]
  pivot_idx <- idx[1]
  non_pivot_idx <- idx[-1]
  pivot_lt <- myquicksort_idx(non_pivot[non_pivot<pivot], decreasing, non_pivot_idx[non_pivot<pivot])
  pivot_ge <- myquicksort_idx(non_pivot[non_pivot>=pivot], decreasing,non_pivot_idx[non_pivot>=pivot])
  if(decreasing){
    sorted_x <- c(pivot_ge$x, pivot, pivot_lt$x)
    sorted_idx <- c(pivot_ge$ix, pivot_idx, pivot_lt$ix)
    return(list('x'=sorted_x, 'ix'=sorted_idx))
  }
  else{
    sorted_x <- c(pivot_lt$x, pivot, pivot_ge$x)
    sorted_idx <- c(pivot_lt$ix, pivot_idx, pivot_ge$ix)
    return(list('x'=sorted_x, 'ix'=sorted_idx))
  }
}

myshellsort <- function(x, decreasing = FALSE, idx){ #Shell sort based on interval of indexes
  n <- length(x)
  interval <- n %/% 2
  if(!decreasing){
    while(interval >= 1){
      for(i in c((interval+1):n)){
        temp <- x[i]
        temp_idx <- idx[i]
        j <- i-interval
        while (j > 0 && temp < x[j]){
          x[j + interval] <- x[j]
          idx[j + interval] <- idx[j]
          x[j] <- temp
          idx[j] <- temp_idx
          j <- j-interval
        }
      }
      interval <- interval %/% 2
    }
  }
  else{
    while(interval >= 1){
      for(i in c((interval+1):n)){
        temp <- x[i]
        temp_idx <- idx[i]
        j <- i-interval
        while (j > 0 && temp > x[j]){
          x[j + interval] <- x[j]
          idx[j + interval] <- idx[j]
          x[j] <- temp
          idx[j] <- temp_idx
          j <- j-interval
        }
      }
      interval <- interval %/% 2
    }
  }
  return(list("x"=x, "ix"=idx))
}

myautosort <- function(x, decreasing = FALSE, idx){ #select sort method, if sample size is small, then use shellsort, otherwise quick sort
  n <- length(x)
  if(n < 2^31){
    return(myshellsort(x, decreasing = decreasing, idx = idx))
  }
  else{
    return(myquicksort_idx(x, decreasing = decreasing, idx = idx))
  }
}
