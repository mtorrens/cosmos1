################################################################################
# source('~/Desktop/project/syntax/00_start.R')
# source('~/Desktop/upf/courses/term3/dstopics/project/syntax/00_start.R')
################################################################################
# source(paste(SRCDIR, '02_final_data.R', sep = ''))
################################################################################

# Load classes
file <- paste(DATDIR, 'train_class.RData', sep = '')
tclass <- get(load(file = file)); cat('Loaded file:', file, '\n')

# Load features
file <- paste(DATDIR, 'X_164int75_120obs.RData', sep = '')
#file <- paste(DATDIR, 'X_164int75_100obs.RData', sep = '')
#file <- paste(DATDIR, 'X_164int75.RData', sep = '')
#file.out <- paste(DATDIR, 'X_164int100.RData', sep = '')
#file.out <- paste(DATDIR, 'X_171ext50.RData', sep = '')
load(file = file); cat('Loaded file:', file, '\n')

# SVD did not work here
#kill <- c(8027, 42621)  # Only for "X_164int100"
kill <- NULL

# Cut
if (length(kill) > 0) {
  y <- tclass[which(tclass[, 'valid'] == TRUE), ][-kill, ]
  X <- X[-kill, ]
  #Z <- as.data.frame(X)
} else {
  y <- tclass[which(tclass[, 'valid'] == TRUE), ]
  #Z <- as.data.frame(X)  
}; gc()

# Save
file <- paste(DATDIR, 'final_data.RData', sep = '')
save(y, X, file = file); cat('Saved file:', file, '\n')

# Clear all memory
rm(list = ls()); gc(); q('no')
# END OF SCRIPT
