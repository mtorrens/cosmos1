################################################################################
# source('~/Desktop/project/syntax/00_start.R')
# source('~/Desktop/upf/courses/term3/dstopics/project/syntax/00_start.R')
################################################################################
# source(paste(SRCDIR, '04_get_results.R', sep = ''))
################################################################################

# Load results
file <- paste(DATDIR, 'results_all.RData', sep = '')
load(file = file); cat('Loaded file:', file, '\n')

# Confusion matrices
(ttL1 <- table(pL1, y[, 2]))
(ttL2 <- table(pL2, y[, 2]))
(ttRF0 <- table(pRF0, y[, 2]))
(ttRFL1 <- table(pRFL1, y[, 2]))
(ttRFL2 <- table(pRFL2, y[, 2]))
(ttRFB1 <- table(pRFB1, y[, 2]))
(ttRFB2 <- table(pRFB2, y[, 2]))
(ttGB0 <- table(pGB0, y[, 2]))
(ttGBL1 <- table(pGBL1, y[, 2]))
(ttGBL2 <- table(pGBL2, y[, 2]))
(ttGBB1 <- table(pGBB1, y[, 2]))
(ttGBB2 <- table(pGBB2, y[, 2]))
(ttDT0 <- table(pDT0, y[, 2]))
(ttDTL1 <- table(pDTL1, y[, 2]))
(ttDTL2 <- table(pDTL2, y[, 2]))
(ttDTB1 <- table(pDTB1, y[, 2]))
(ttDTB2 <- table(pDTB2, y[, 2]))
(ttSVM0 <- table(pSVM0, y[, 2]))
(ttSVML1 <- table(pSVML1, y[, 2]))
(ttSVML2 <- table(pSVML2, y[, 2]))
(ttSVMB1 <- table(pSVMB1, y[, 2]))
(ttSVMB2 <- table(pSVMB2, y[, 2]))
(ttKNN0 <- table(pKNN0, y[, 2]))
(ttKNNL1 <- table(pKNNL1, y[, 2]))
(ttKNNL2 <- table(pKNNL2, y[, 2]))
(ttKNNB1 <- table(pKNNB1, y[, 2]))
(ttKNNB2 <- table(pKNNB2, y[, 2]))

# Predictive accuracies
accL1 <- sum(diag(ttL1)) / sum(ttL1)
accL2 <- sum(diag(ttL2)) / sum(ttL2)
accRF0 <- sum(diag(ttRF0)) / sum(ttRF0)
accRFL1 <- sum(diag(ttRFL1)) / sum(ttRFL1)
accRFL2 <- sum(diag(ttRFL2)) / sum(ttRFL2)
accRFB1 <- sum(diag(ttRFB1)) / sum(ttRFB1)
accRFB2 <- sum(diag(ttRFB2)) / sum(ttRFB2)
accGB0 <- sum(diag(ttGB0)) / sum(ttGB0)
accGBL1 <- sum(diag(ttGBL1)) / sum(ttGBL1)
accGBL2 <- sum(diag(ttGBL2)) / sum(ttGBL2)
accGBB1 <- sum(diag(ttGBB1)) / sum(ttGBB1)
accGBB2 <- sum(diag(ttGBB2)) / sum(ttGBB2)
accDT0 <- sum(diag(ttDT0)) / sum(ttDT0)
accDTL1 <- sum(diag(ttDTL1)) / sum(ttDTL1)
accDTL2 <- sum(diag(ttDTL2)) / sum(ttDTL2)
accDTB1 <- sum(diag(ttDTB1)) / sum(ttDTB1)
accDTB2 <- sum(diag(ttDTB2)) / sum(ttDTB2)
accSVM0 <- sum(diag(ttSVM0)) / sum(ttSVM0)
accSVML1 <- sum(diag(ttSVML1)) / sum(ttSVML1)
accSVML2 <- sum(diag(ttSVML2)) / sum(ttSVML2)
accSVMB1 <- sum(diag(ttSVMB1)) / sum(ttSVMB1)
accSVMB2 <- sum(diag(ttSVMB2)) / sum(ttSVMB2)
accKNN0 <- sum(diag(ttKNN0)) / sum(ttKNN0)
accKNNL1 <- sum(diag(ttKNNL1)) / sum(ttKNNL1)
accKNNL2 <- sum(diag(ttKNNL2)) / sum(ttKNNL2)
accKNNB1 <- sum(diag(ttKNNB1)) / sum(ttKNNB1)
accKNNB2 <- sum(diag(ttKNNB2)) / sum(ttKNNB2)

# Print results
d <- 3
cat('* ACCURACIES:\n')
cat('* Lasso (two-p): L1 = ', round(accL1, d),
    '; L2 = ', round(accL2, d), '\n', sep = '')
cat('* Random Forest: RF0  = ', round(accRF0, d), '\n',
    '                 RFL1 = ', round(accRFL1, d), '; RFL2 = ', round(accRFL2, d), '\n',
    '                 RFB1 = ', round(accRFB1, d), '; RFB2 = ', round(accRFB2, d), '\n', sep = '')
cat('* Gradi. Boost.: GB0  = ', round(accGB0, d), '\n',
    '                 GBL1 = ', round(accGBL1, d), '; GBL2 = ', round(accGBL2, d), '\n',
    '                 GBB1 = ', round(accGBB1, d), '; GBB2 = ', round(accGBB2, d), '\n', sep = '')
cat('* Decision Tree: DT0  = ', round(accDT0, d), '\n',
    '                 DTL1 = ', round(accDTL1, d), '; DTL2 = ', round(accDTL2, d), '\n',
    '                 DTB1 = ', round(accDTB1, d), '; DTB2 = ', round(accDTB2, d), '\n', sep = '')
cat('* Supp Vec Mach: SVM0  = ', round(accSVM0, d), '\n',
    '                 SVML1 = ', round(accSVML1, d), '; SVML2 = ', round(accSVML2, d), '\n',
    '                 SVMB1 = ', round(accSVMB1, d), '; SVMB2 = ', round(accSVMB2, d), '\n', sep = '')
cat('* K-Near-Neighb: KNN0  = ', round(accKNN0, d), '\n',
    '                 KNNL1 = ', round(accKNNL1, d), '; KNNL2 = ', round(accKNNL2, d), '\n',
    '                 KNNB1 = ', round(accKNNB1, d), '; KNNB2 = ', round(accKNNB2, d), '\n', sep = '')

# Number of predictors
round(mean(numvL1U), 1)
round(mean(numvL2U), 1)
round(mean(numvB1U), 1)
round(mean(numvB2U), 1)

# Best results
ttRF0
ttRFB2
ttGB0
# END OF SCRIPT
