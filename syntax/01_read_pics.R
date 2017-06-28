################################################################################
# source('~/Desktop/project/syntax/00_start.R')
# source('~/Desktop/upf/courses/term3/dstopics/project/syntax/00_start.R')
################################################################################
# source(paste(SRCDIR, '01_read_pics.R', sep = ''))
################################################################################

# Parameters
force <- FALSE

# Classify sample
file.out <- paste(DATDIR, 'train_class.RData', sep = '')
if (! file.exists(file.out) || force == TRUE) {
  # Load data on human answers to questions
  file.trainqs <- paste(INPDIR, 'training_solutions_rev1.csv', sep = '')
  train.qs <- read.csv(file = file.trainqs, sep = ',')

  # Classify the images in one of the six categories
  train.class <- tag.galaxy(train.qs, reduce = TRUE, n.cat = 120, cats = 5)

  # Save data
  save(train.class, file = file.out); cat('Saved file:', file.out, '\n')
} else {
  train.class <- get(load(file = file.out)); cat('Loaded file:', file.out, '\n')
}

# Summary results
table(train.class[, 2]) / sum(table(train.class[, 2]))
ch <- which(train.class[, 'valid'] == TRUE)
tapply(train.class[ch, 'prob'], train.class[ch, 'type'], mean)

# Convert images to processed matrix
file.out <- paste(DATDIR, 'X_164int75_120obs.RData', sep = '')
#file.out <- paste(DATDIR, 'X_164int100.RData', sep = '')
#file.out <- paste(DATDIR, 'X_171ext50.RData', sep = '')
if (! file.exists(file.out) || force == TRUE) {
  # Generate X matrix
  Z <- preproc.img(type = train.class[, 'type'],    # Classification
                   valid = train.class[, 'valid'],  # Which ones are valid
                   extensive = FALSE,
                   fix.width = TRUE,
                   pc.kept = 75)

  # Disentangle data
  X <- Z[[1]]
  exp.var <- Z[[2]]

  # Average percentage of explained variance
  cat('* Average Proportion of Explained Variance:', mean(exp.var), '\n')
  quantile(exp.var, probs = c(0.025, 0.975))

  # Save result
  save(X, file = file.out); cat('Saved file:', file.out, '\n')
}
# END OF SCRIPT
