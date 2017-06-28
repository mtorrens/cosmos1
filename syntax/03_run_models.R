################################################################################
# source('~/Desktop/project/syntax/00_start.R')
# source('~/Desktop/upf/courses/term3/dstopics/project/syntax/00_start.R')
################################################################################
# source(paste(SRCDIR, '03_run_models.R', sep = ''))
################################################################################

# Load data
file <- paste(DATDIR, 'final_data.RData', sep = '')
load(file = file); cat('Loaded file:', file, '\n')

# Parameters
n <- nrow(X)
p <- ncol(X)

# Unified matrix
Z <- cbind.data.frame(y[, 'type'], as.data.frame(X))
colnames(Z)[1] <- 'y'

# Priors
pm <- mombf::modelunifprior()
pv <- mombf::igprior(0.01, 0.01)
#pb <- mombf::momprior(tau = 0.348)

# Functions
ms <- mombf::modelSelection

################################################################################
# Models
################################################################################
# * Random Forest (randomForest)
# * Gradient Boosting (xgboost)
# * Decision Trees (rpart)
# * SVM (e1071)
# * KNN (class)
### Models disregarded (comput.) ###############################################
# * AdaBoost (adabag)
# * Neural Network (neuralnet, nnet)
# * Other: SGD (sgd)
################################################################################

# Setup
nfold <- 10  # Number of folds for k-fold CV
folds <- kfold(n = n, k = nfold, seed = 666)

# Initialise vectors
pL1 <- pL2 <- rep(NA, n)
pRF0 <- pRFL1 <- pRFL2 <- pRFB1 <- pRFB2 <- rep(NA, n)
pGB0 <- pGBL1 <- pGBL2 <- pGBB1 <- pGBB2 <- rep(NA, n)
pDT0 <- pDTL1 <- pDTL2 <- pDTB1 <- pDTB2 <- rep(NA, n)
pSVM0 <- pSVML1 <- pSVML2 <- pSVMB1 <- pSVMB2 <- rep(NA, n)
pKNN0 <- pKNNL1 <- pKNNL2 <- pKNNB1 <- pKNNB2 <- rep(NA, n)
numvB1U <- numvB1I <- numvB2U <- numvB2I <- numvL1U <- numvL2U <- rep(NA, nfold)

# k-fold CV (intentionally not parallelised)
set.seed(666)
for (i in 1:nfold) {
  # Iteration
  cat('* FOLD No.:', i, 'OF', nfold, '\n')
  inc <- (1:n)[! (1:n) %in% folds[[i]]]
  ni <- nrow(X[inc, ])

  ################################################################################
  # Bayesian Variable Selection
  ################################################################################
  L1 <- 0.05
  L2 <- 0.06
  
  # Binary outcomes
  isD <- matrix(as.numeric(y[inc, 'type'] == 'D'), ncol = 1)
  isE <- matrix(as.numeric(y[inc, 'type'] == 'E'), ncol = 1)
  isM <- matrix(as.numeric(y[inc, 'type'] == 'M'), ncol = 1)
  isR <- matrix(as.numeric(y[inc, 'type'] == 'R'), ncol = 1)
  isS <- matrix(as.numeric(y[inc, 'type'] == 'S'), ncol = 1)

  # Run various binary models
  mbD <- pmomLM(y = isD, x = X[inc, ], xadj = rep(1, ni), niter = 1e4,
                priorDelta = pm, priorVar = pv, center = FALSE, scale = FALSE)
  mbE <- pmomLM(y = isE, x = X[inc, ], xadj = rep(1, ni), niter = 1e4,
                priorDelta = pm, priorVar = pv, center = FALSE, scale = FALSE)
  mbM <- pmomLM(y = isM, x = X[inc, ], xadj = rep(1, ni), niter = 1e4,
                priorDelta = pm, priorVar = pv, center = FALSE, scale = FALSE)
  mbR <- pmomLM(y = isR, x = X[inc, ], xadj = rep(1, ni), niter = 1e4,
                priorDelta = pm, priorVar = pv, center = FALSE, scale = FALSE)
  mbS <- pmomLM(y = isS, x = X[inc, ], xadj = rep(1, ni), niter = 1e4,
                priorDelta = pm, priorVar = pv, center = FALSE, scale = FALSE)
  # mbD <- pmomPM(y = isD, x = X[inc, ], xadj = rep(1, ni), niter = 1e4,
  #               priorDelta = pm)
  # mbE <- pmomPM(y = isE, x = X[inc, ], xadj = rep(1, ni), niter = 1e4,
  #               priorDelta = pm)
  # mbM <- pmomPM(y = isM, x = X[inc, ], xadj = rep(1, ni), niter = 1e4,
  #               priorDelta = pm)
  # mbR <- pmomPM(y = isR, x = X[inc, ], xadj = rep(1, ni), niter = 1e4,
  #               priorDelta = pm)
  # mbS <- pmomPM(y = isS, x = X[inc, ], xadj = rep(1, ni), niter = 1e4,
  #               priorDelta = pm)

  # Keep relevant regressors
  varsD1 <- which(mbD[['margpp']] > L1)
  varsD2 <- which(mbD[['margpp']] > L2)
  varsE1 <- which(mbD[['margpp']] > L1)
  varsE2 <- which(mbD[['margpp']] > L2)
  varsM1 <- which(mbD[['margpp']] > L1)
  varsM2 <- which(mbD[['margpp']] > L2)
  varsR1 <- which(mbD[['margpp']] > L1)
  varsR2 <- which(mbD[['margpp']] > L2)
  varsS1 <- which(mbD[['margpp']] > L1)
  varsS2 <- which(mbD[['margpp']] > L2)

  # At different levels of confidence (union and intersection)
  varsB1U <- unique(c(varsD1, varsE1, varsM1, varsR1, varsS1))  # 5%
  varsB2U <- unique(c(varsD2, varsE2, varsM2, varsR2, varsS2))  # 6%
  varsB1I <- c()
  varsB2I <- c()
  for (j in 1:ncol(X)) {
    if ((j %in% varsD1) && (j %in% varsE1) && (j %in% varsM1) &&
        (j %in% varsR1) && (j %in% varsS1)) {
      varsB1I <- c(varsB1I, j)  # 5%
    }
    if ((j %in% varsD2) && (j %in% varsE2) && (j %in% varsM2) &&
        (j %in% varsR2) && (j %in% varsS2)) {
      varsB2I <- c(varsB2I, j)  # 6%
    }
  }

  # Counts of variables
  nvB1U <- length(varsB1U)
  nvB2U <- length(varsB2U)
  nvB1I <- length(varsB1I)
  nvB2I <- length(varsB2I)

  ################################################################################
  # Multinomial Lasso
  ################################################################################
  # Model
  multL <- cv.glmnet(x = X[inc, ], y = as.factor(y[inc, 2]),
                     family = 'multinomial', type.multinomial = 'grouped')

  # Predictors kept for two lambdas (union and intersection)
  coefs1 <- coef(multL, s = 'lambda.min')
  coefs2 <- coef(multL, s = 'lambda.1se')
  varsL1U <- c()
  varsL2U <- c()
  for (m in 1:length(coefs1)) {
    varsL1U <- c(varsL1U, which(coefs1[[m]] != 0) - 1)
  }
  for (m in 1:length(coefs2)) {
    varsL2U <- c(varsL2U, which(coefs2[[m]] != 0) - 1)
  }
  #varsL1I <- names(table(varsL1U) == length(coefs1))  # Same
  #varsL2I <- names(table(varsL2U) == length(coefs2))
  varsL1U <- unique(varsL1U[varsL1U != 0])
  varsL2U <- unique(varsL2U[varsL2U != 0])

  # Counts of variables
  nvL1U <- length(varsL1U)
  nvL2U <- length(varsL2U)

  # Lasso predictions for two standard lambda choices
  predsL1 <- predict(multL, X[folds[[i]], ], type = 'class', s = 'lambda.min')
  predsL2 <- predict(multL, X[folds[[i]], ], type = 'class', s = 'lambda.1se')

  ##############################################################################
  # 1. Random Forest
  ##############################################################################
  # Full Random Forest
  rf0 <- randomForest(y = as.factor(y[inc, 2]), x = X[inc, ])
  predsRF0 <- predict(rf0, newdata = X[folds[[i]], ])

  # Random Forest with Lasso 1 predictors
  rfL1 <- randomForest(y = as.factor(y[inc, 2]), x = X[inc, varsL1U])
  predsRFL1 <- predict(rfL1, newdata = X[folds[[i]], varsL1U])

  # Random Forest with Lasso 2 predictors
  rfL2 <- randomForest(y = as.factor(y[inc, 2]), x = X[inc, varsL2U])
  predsRFL2 <- predict(rfL2, newdata = X[folds[[i]], varsL2U])

  # Random Forest with Bayesian 1U
  rfB1U <- randomForest(y = as.factor(y[inc, 2]), x = X[inc, varsB1U])
  predsRFB1U <- predict(rfB1U, newdata = X[folds[[i]], varsB1U])

  # # Random Forest with Bayesian 1I
  # rfB1I <- randomForest(y = as.factor(y[inc, 2]), x = X[inc, varsB1I])
  # predsRFB1I <- predict(rfB1I, newdata = X[folds[[i]], varsB1I])

  # Random Forest with Bayesian 2U
  rfB2U <- randomForest(y = as.factor(y[inc, 2]), x = X[inc, varsB2U])
  predsRFB2U <- predict(rfB2U, newdata = X[folds[[i]], varsB2U])

  # # Random Forest with Bayesian 1I
  # rfB2I <- randomForest(y = as.factor(y[inc, 2]), x = X[inc, varsB2I])
  # predsRFB2I <- predict(rfB2I, newdata = X[folds[[i]], varsB2I])

  ##############################################################################
  # 2. Gradient Boosting
  ##############################################################################
  # Full GB model
  Zm <- xgb.DMatrix(data = X[inc, ], label = as.numeric(as.factor(y[inc, 2])))
  Xm <- xgb.DMatrix(data = X[folds[[i]], ])
  pars <- list('eval_metric' = 'mlogloss', 'objective' = 'multi:softmax',
               'num_class' = length(unique(as.factor(y[inc, 2]))) + 1)
  gb0 <- xgboost::xgboost(data = Zm, params = pars, nrounds = 50, verbose = 0)
  predsGB0 <- as.character(predict(gb0, newdata = Xm))
  for (num in 1:length(predsGB0)) {
    predsGB0[num] <- switch(predsGB0[num], '3' = 'M',
                                           '2' = 'E',
                                           '5' = 'S',
                                           '4' = 'R',
                                           '1' = 'D')
  }

  # GB with Lasso 1 predictors
  Xm <- xgb.DMatrix(data = X[folds[[i]], varsL1U])
  Zm <- xgb.DMatrix(data = X[inc, varsL1U],
                    label = as.numeric(as.factor(y[inc, 2])))
  gbL1 <- xgboost(data = Zm, params = pars, nrounds = 50, verbose = 0)
  predsGBL1 <- as.character(predict(gbL1, newdata = Xm))
  for (num in 1:length(predsGBL1)) {
    predsGBL1[num] <- switch(predsGBL1[num], '3' = 'M',
                                             '2' = 'E',
                                             '5' = 'S',
                                             '4' = 'R',
                                             '1' = 'D')
  }

  # GB with Lasso 2 predictors
  Xm <- xgb.DMatrix(data = X[folds[[i]], varsL2U])
  Zm <- xgb.DMatrix(data = X[inc, varsL2U],
                    label = as.numeric(as.factor(y[inc, 2])))
  gbL2 <- xgboost::xgboost(data = Zm, params = pars, nrounds = 50, verbose = 0)
  predsGBL2 <- as.character(predict(gbL2, newdata = Xm))
  for (num in 1:length(predsGBL2)) {
    predsGBL2[num] <- switch(predsGBL2[num], '3' = 'M',
                                             '2' = 'E',
                                             '5' = 'S',
                                             '4' = 'R',
                                             '1' = 'D')
  }

  # GB with Bayesian 1U
  Xm <- xgb.DMatrix(data = X[folds[[i]], varsB1U])
  Zm <- xgb.DMatrix(data = X[inc, varsB1U],
                    label = as.numeric(as.factor(y[inc, 2])))
  gbB1U <- xgboost::xgboost(data = Zm, params = pars, nrounds = 50, verbose = 0)
  predsGBB1U <- as.character(predict(gbB1U, newdata = Xm))
  for (num in 1:length(predsGBB1U)) {
    predsGBB1U[num] <- switch(predsGBB1U[num], '3' = 'M',
                                               '2' = 'E',
                                               '5' = 'S',
                                               '4' = 'R',
                                               '1' = 'D')
  }

  # GB with Bayesian 2U
  Xm <- xgb.DMatrix(data = X[folds[[i]], varsB2U])
  Zm <- xgb.DMatrix(data = X[inc, varsB2U],
                    label = as.numeric(as.factor(y[inc, 2])))
  gbB2U <- xgboost::xgboost(data = Zm, params = pars, nrounds = 50, verbose = 0)
  predsGBB2U <- as.character(predict(gbB2U, newdata = Xm))
  for (num in 1:length(predsGBB2U)) {
    predsGBB2U[num] <- switch(predsGBB2U[num], '3' = 'M',
                                               '2' = 'E',
                                               '5' = 'S',
                                               '4' = 'R',
                                               '1' = 'D')
  }

  # ##############################################################################
  # # 2B. AdaBoost
  # ##############################################################################
  # # Full adaboost
  # predictB <- predict.boosting  # Just for short
  # ab0 <- adabag::boosting(as.factor(y) ~ ., data = Z[inc, ])
  # predsAB0 <- predictB(ab0, newdata = Z[folds[[i]], ])

  # # AdaBoost with Lasso 1 predictors
  # abL1 <- boosting(as.factor(y) ~ ., data = Z[inc, c(1, varsL1U + 1)])
  # predsABL1 <- predictB(abL1, newdata = Z[folds[[i]], c(1, varsL1U + 1)])

  # # AdaBoost with Lasso 2 predictors
  # abL2 <- boosting(as.factor(y) ~ ., data = Z[inc, c(1, varsL2U + 1)])
  # predsABL2 <- predictB(abL2, newdata = Z[folds[[i]], c(1, varsL2U + 1)])

  # # AdaBoost with Bayesian 1U
  # abB1U <- boosting(as.factor(y) ~ ., data = Z[inc, c(1, varsB1U + 1)])
  # predsABB1U <- predictB(abB1U, newdata = Z[folds[[i]], c(1, varsB1U + 1)])

  # # AdaBoost with Bayesian 2U
  # abB2U <- boosting(as.factor(y) ~ ., data = Z[inc, c(1, varsB2U + 1)])
  # predsABB2U <- predictB(abB2U, newdata = Z[folds[[i]], c(1, varsB2U + 1)])

  ##############################################################################
  # 3. SVM
  ##############################################################################
  # Full SVM
  svm0 <- e1071::svm(as.factor(y) ~ ., data = Z[inc, ], kernel = 'radial')
  predsSVM0 <- predict(svm0, newdata = Z[folds[[i]], ])

  # SVM with Lasso 1 predictors
  svmL1 <- svm(as.factor(y) ~ ., data = Z[inc, c(1, varsL1U + 1)])
  predsSVML1 <- predict(svmL1, newdata = Z[folds[[i]], c(1, varsL1U + 1)])

  # SVM with Lasso 2 predictors
  svmL2 <- svm(as.factor(y) ~ ., data = Z[inc, c(1, varsL2U + 1)])
  predsSVML2 <- predict(svmL2, newdata = Z[folds[[i]], c(1, varsL2U + 1)])

  # SVM with Bayesian 1U
  svmB1U <- svm(as.factor(y) ~ ., data = Z[inc, c(1, varsB1U + 1)])
  predsSVMB1U <- predict(svmB1U, newdata = Z[folds[[i]], c(1, varsB1U + 1)])

  # SVM with Bayesian 2U
  svmB2U <- svm(as.factor(y) ~ ., data = Z[inc, c(1, varsB2U + 1)])
  predsSVMB2U <- predict(svmB2U, newdata = Z[folds[[i]], c(1, varsB2U + 1)])

  ##############################################################################
  # 4. Recursive partition trees
  ##############################################################################
  # Full Partition Tree
  dt0 <- rpart::rpart(as.factor(y) ~ ., data = Z[inc, ])
  aux <- predict(dt0, newdata = Z[folds[[i]], ])
  predsDT0 <- colnames(aux)[apply(aux, 1, which.max)]

  # Partition Tree with Lasso 1 predictors
  dtL1 <- rpart(as.factor(y) ~ ., data = Z[inc, c(1, varsL1U + 1)])
  aux <- predict(dtL1, newdata = Z[folds[[i]], c(1, varsL1U + 1)])
  predsDTL1 <- colnames(aux)[apply(aux, 1, which.max)]

  # Partition Tree with Lasso 2 predictors
  dtL2 <- rpart(as.factor(y) ~ ., data = Z[inc, c(1, varsL2U + 1)])
  aux <- predict(dtL2, newdata = Z[folds[[i]], c(1, varsL2U + 1)])
  predsDTL2 <- colnames(aux)[apply(aux, 1, which.max)]

  # Partition Tree with Bayesian 1U
  dtB1U <- rpart(as.factor(y) ~ ., data = Z[inc, c(1, varsB1U + 1)])
  aux <- predict(dtB1U, newdata = Z[folds[[i]], c(1, varsB1U + 1)])
  predsDTB1U <- colnames(aux)[apply(aux, 1, which.max)]

  # Partition Tree with Bayesian 2U
  dtB2U <- rpart(as.factor(y) ~ ., data = Z[inc, c(1, varsB2U + 1)])
  aux <- predict(dtB2U, newdata = Z[folds[[i]], c(1, varsB2U + 1)])
  predsDTB2U <- colnames(aux)[apply(aux, 1, which.max)]

  ##############################################################################
  # 5. k-NN
  ##############################################################################
  # Full k-NN
  predsKNN0 <- class::knn(train = X[inc, ], cl = as.factor(y[inc, 2]),
                          test = X[folds[[i]], ], k = 3)

  # k-NN with Lasso 1 predictors
  predsKNNL1 <- knn(train = X[inc, varsL1U], cl = as.factor(y[inc, 2]),
                    test = X[folds[[i]], varsL1U], k = 3)

  # SVM with Lasso 2 predictors
  predsKNNL2 <- knn(train = X[inc, varsL2U], cl = as.factor(y[inc, 2]),
                    test = X[folds[[i]], varsL2U], k = 3)

  # SVM with Bayesian 1U
  predsKNNB1U <- knn(train = X[inc, varsB1U], cl = as.factor(y[inc, 2]),
                     test = X[folds[[i]], varsB1U], k = 3)

  # SVM with Bayesian 2U
  predsKNNB2U <- knn(train = X[inc, varsB2U], cl = as.factor(y[inc, 2]),
                     test = X[folds[[i]], varsB2U], k = 3)

  # ##############################################################################
  # # 6. Neural networks
  # ##############################################################################
  # # Entire Neural Network
  # #f0 <- as.formula(paste('y ~', paste(colnames(Z)[2:ncol(Z)], collapse = '+')))
  # #nn0 <- neuralnet(f0, data = Z[inc, ], hidden = 1, linear.output = FALSE)
  # nn0 <- nnet::nnet(as.factor(y) ~ ., data = Z[inc, ], size = 3)
  # aux <- predict(nn0, newdata = Z[folds[[i]], ])
  # predsNN0 <- colnames(aux)[apply(aux, 1, which.max)]

  # # Neural Network with Lasso 1 predictors
  # nnL1 <- nnet(as.factor(y) ~ ., data = Z[inc, c(1, varsL1U + 1)], size = 5)
  # aux <- predict(nnL1, newdata = Z[folds[[i]], c(1, varsL1U + 1)])
  # predsNNL1 <- colnames(aux)[apply(aux, 1, which.max)]

  # # Neural Network with Lasso 2 predictors
  # nnL2 <- nnet(as.factor(y) ~ ., data = Z[inc, c(1, varsL2U + 1)], size = 5)
  # aux <- predict(nnL2, newdata = Z[folds[[i]], c(1, varsL2U + 1)])
  # predsNNL2 <- colnames(aux)[apply(aux, 1, which.max)]

  # # Neural Network with Bayesian 1U
  # nnB1U <- nnet(as.factor(y) ~ ., data = Z[inc, c(1, varsB1U + 1)], size = 5)
  # aux <- predict(nnB1U, newdata = Z[folds[[i]], c(1, varsB1U + 1)])
  # predsNNB1U <- colnames(aux)[apply(aux, 1, which.max)]

  # # Neural Network with Bayesian 2U
  # nnB2U <- nnet(as.factor(y) ~ ., data = Z[inc, c(1, varsB2U + 1)], size = 5)
  # aux <- predict(nnB2U, newdata = Z[folds[[i]], c(1, varsB2U + 1)])
  # predsNNB2U <- colnames(aux)[apply(aux, 1, which.max)]

  ##############################################################################
  # Assign results
  ##############################################################################
  # Predictions
  pL1[folds[[i]]] <- predsL1
  pL2[folds[[i]]] <- predsL2
  pRF0[folds[[i]]] <- predsRF0
  pRFL1[folds[[i]]] <- predsRFL1
  pRFL2[folds[[i]]] <- predsRFL2
  pRFB1[folds[[i]]] <- predsRFB1U
  pRFB2[folds[[i]]] <- predsRFB2U
  pGB0[folds[[i]]] <- predsGB0
  pGBL1[folds[[i]]] <- predsGBL1
  pGBL2[folds[[i]]] <- predsGBL2
  pGBB1[folds[[i]]] <- predsGBB1U
  pGBB2[folds[[i]]] <- predsGBB2U
  pDT0[folds[[i]]] <- predsDT0
  pDTL1[folds[[i]]] <- predsDTL1
  pDTL2[folds[[i]]] <- predsDTL2
  pDTB1[folds[[i]]] <- predsDTB1U
  pDTB2[folds[[i]]] <- predsDTB2U
  pSVM0[folds[[i]]] <- predsSVM0
  pSVML1[folds[[i]]] <- predsSVML1
  pSVML2[folds[[i]]] <- predsSVML2
  pSVMB1[folds[[i]]] <- predsSVMB1U
  pSVMB2[folds[[i]]] <- predsSVMB2U
  pKNN0[folds[[i]]] <- predsKNN0
  pKNNL1[folds[[i]]] <- predsKNNL1
  pKNNL2[folds[[i]]] <- predsKNNL2
  pKNNB1[folds[[i]]] <- predsKNNB1U
  pKNNB2[folds[[i]]] <- predsKNNB2U
  # pNN0[folds[[i]]] <- predsNN0
  # pNNL1[folds[[i]]] <- predsNNL1
  # pNNL2[folds[[i]]] <- predsNNL2
  # pNNB1[folds[[i]]] <- predsNNB1U
  # pNNB2[folds[[i]]] <- predsNNB2U

  # Counts
  numvB1U[i] <- nvB1U
  numvB1I[i] <- nvB1I
  numvB2U[i] <- nvB2U
  numvB2I[i] <- nvB2I
  numvL1U[i] <- nvL1U
  numvL2U[i] <- nvL2U

  # Save results
  file <- paste(TMPDIR, 'results_iter', i, '.RData', sep = '')
  save.image(file = file); cat('Saved file:', file, '\n')
}; cat('\n')

# Save data
file <- paste(DATDIR, 'results_all.RData', sep = '')
save.image(file = file); cat('Saved file:', file, '\n')
# END OF SCRIPT
