time_block_diffs <- function(i) c(paste("added", setdiff(s[[i]], s[[i-1]])),
                       
                       paste("removed", setdiff(s[[i-1]], s[[i]])), 
                       
                       paste("backward", intersect(s[[i]], s[[i-1]])),
                       
                       paste("forward", intersect(s[[i]], s[[i+1]]))
)