################################################################################
angle <- function(x, y){
################################################################################
  dot.prod <- x %*% y 
  norm.x <- norm(x, type = '2')
  norm.y <- norm(y, type = '2')
  theta <- acos(dot.prod / (norm.x * norm.y))
  as.numeric(theta)
}

################################################################################
tag.galaxy <- function(X, reduce = FALSE, n.cat = 1500, cats = 7) {
################################################################################
  # Sanity check
  if (ncol(X) != 38) {
    stop('not all qualitative questions available.')
  }

  # Classify
  r <- which(X[, 'Class1.1'] > X[, 'Class1.2'] &
             X[, 'Class1.1'] > X[, 'Class1.3'] &
             X[, 'Class7.1'] > X[, 'Class7.2'] &
             X[, 'Class7.1'] > X[, 'Class7.3'] &
             X[, 'Class6.2'] > X[, 'Class6.1'])
  e <- which(X[, 'Class1.1'] > X[, 'Class1.2'] &
             X[, 'Class1.1'] > X[, 'Class1.3'] &
            (X[, 'Class7.2'] > X[, 'Class7.1'] |
             X[, 'Class7.3'] > X[, 'Class7.1']) &
             X[, 'Class6.2'] > X[, 'Class6.1'])
  d <- which(X[, 'Class1.2'] > X[, 'Class1.1'] &
             X[, 'Class1.2'] > X[, 'Class1.3'] &
             X[, 'Class1.2'] > X[, 'Class1.1'] &
             X[, 'Class2.1'] > X[, 'Class2.2'] &
             X[, 'Class6.2'] > X[, 'Class6.1'])
  s <- which(X[, 'Class1.2'] > X[, 'Class1.1'] &
             X[, 'Class1.2'] > X[, 'Class1.3'] &
             X[, 'Class1.2'] > X[, 'Class1.1'] &
             X[, 'Class2.2'] > X[, 'Class2.1'] &
             X[, 'Class4.1'] > X[, 'Class4.2'] &
             X[, 'Class6.2'] > X[, 'Class6.1'])
  b <- which(X[, 'Class1.2'] > X[, 'Class1.1'] &
             X[, 'Class1.2'] > X[, 'Class1.3'] &
             X[, 'Class2.2'] > X[, 'Class2.1'] &
             X[, 'Class3.1'] > X[, 'Class3.2'] &
             X[, 'Class4.2'] > X[, 'Class4.1'] &
             X[, 'Class6.2'] > X[, 'Class6.1'])
  o <- which(X[, 'Class1.3'] > X[, 'Class1.1'] &
             X[, 'Class1.3'] > X[, 'Class1.2'])
  m <- which(X[, 'Class6.1'] > X[, 'Class6.2'] &
             X[, 'Class8.6'] > X[, 'Class8.1'] &
             X[, 'Class8.6'] > X[, 'Class8.2'] &
             X[, 'Class8.6'] > X[, 'Class8.3'] &
             X[, 'Class8.6'] > X[, 'Class8.4'] &
             X[, 'Class8.6'] > X[, 'Class8.5'] &
             X[, 'Class8.6'] > X[, 'Class8.7'])
  m <- m[! m %in% o]
  w <- which(X[, 'Class6.1'] > X[, 'Class6.2'])
  w <- w[! w %in% m]
  w <- w[! w %in% o]

  # Tag
  tag <- rep(NA, nrow(X))
  tag[r] <- 'R'
  tag[e] <- 'E'
  tag[d] <- 'D'
  tag[s] <- 'S'
  tag[b] <- 'B'
  tag[m] <- 'M'
  tag[o] <- 'O'
  tag[w] <- 'W'

  # Actual probability
  prob <- rep(0, nrow(X))
  prob[r] <- X[r, 'Class7.1']
  prob[e] <- X[e, 'Class7.2'] + X[e, 'Class7.3']
  prob[d] <- X[d, 'Class2.1']
  prob[s] <- X[s, 'Class4.1']
  prob[b] <- X[b, 'Class3.1'] * X[b, 'Class4.2']
  prob[m] <- X[m, 'Class8.6']
  prob[o] <- X[o, 'Class1.3'] 
  prob[w] <- X[w, 'Class6.1'] - X[w, 'Class8.6']

  # Valid observations
  if (reduce == FALSE) {
    valid <- rep(TRUE, nrow(X))
    valid[which(is.na(tag) | tag == 'O')] <- FALSE
  } else {
    valid <- rep(FALSE, nrow(X))
    valid[which(X[, 1] %in% X[r, 1][order(prob[r], decreasing = TRUE)[1:n.cat]])] <- TRUE
    valid[which(X[, 1] %in% X[e, 1][order(prob[e], decreasing = TRUE)[1:n.cat]])] <- TRUE
    valid[which(X[, 1] %in% X[d, 1][order(prob[d], decreasing = TRUE)[1:n.cat]])] <- TRUE
    valid[which(X[, 1] %in% X[s, 1][order(prob[s], decreasing = TRUE)[1:n.cat]])] <- TRUE
    valid[which(X[, 1] %in% X[b, 1][order(prob[b], decreasing = TRUE)[1:n.cat]])] <- TRUE
    valid[which(X[, 1] %in% X[m, 1][order(prob[m], decreasing = TRUE)[1:n.cat]])] <- TRUE
    valid[which(X[, 1] %in% X[w, 1][order(prob[w], decreasing = TRUE)[1:n.cat]])] <- TRUE
  }

  # The two least important go out
  if (cats == 5) {
    valid[w] <- FALSE
    valid[b] <- FALSE
  }

  # End
  out <- data.frame(id = X[, 1], type = as.character(tag), valid = valid, prob = prob)
  out[, 'type'] <- as.character(out[, 'type'])
  return(out)
}

################################################################################
preproc.img <- function(type, valid, extensive = FALSE, pc.kept = 100,
                        min.int = 0.1, fix.width = TRUE, width = 164,
                        perc.out = 0.3, pix = 0.25, rot.eval = 0.2) {
################################################################################
  # Image list
  img.list <- list.files(paste(INPDIR, 'img_train/', sep = ''))
  img.list <- img.list[which(valid == TRUE)]
  n <- length(img.list)

  # Read raw images
  evar <- rep(NA, n)  # Vector to store explained variance
  for (i in 1:n) {
    # Load image
    cat('\rImage:', i, 'of', n)
    img <- jpeg::readJPEG(paste(INPDIR, 'img_train/', img.list[i], sep = ''))
    #plot(1:2, type = 'n'); rasterImage(img, 1, 1, 2, 2)

    # 1. Crop image
    pxs <- dim(img)[1]
    if (fix.width == TRUE) {
      lims <- c((pxs - width) / 2, pxs - 1 - (pxs - width) / 2)
    } else {
      lims <- c(floor(perc.out * pxs), ceiling((1 - perc.out) * pxs))
    }
    cr.img <- img[lims[1]:lims[2], lims[1]:lims[2], ]
    #cr.img[which(cr.img < min.int * 2)] <- 0
    cr.img[which(cr.img < min.int)] <- 0
    #plot(1:2, type = 'n'); rasterImage(cr.img, 1, 1, 2, 2)

    # 3. Rotate
    np <- nrow(cr.img)
    x0 <- round(0.5 * np)
    x1 <- round(rot.eval * np)
    x2 <- round((1 - rot.eval) * np)
    plain <- max(cr.img[x1, , 1])
    y1 <- which.max(cr.img[x1, , 1])
    y2 <- which.max(cr.img[x2, , 1])
    a <- (y2 - y1) / (x2 - x1)  # Slope
    b <- y2 - a * x2
    y0 <- round(a * x0 + b)    
    xA <- (np - x1) - (np - x0)
    yA <- y1 - y0
    xB <- xA
    yB <- 0
    rad <- angle(c(xA, yA), c(xB, yB))
    th <- ifelse(a > 0, -(rad * 180) / pi, (rad * 180) / pi)
    if (plain == 0) { th <- 90 }
    ro.img <- EBImage::rotate(img, th)
    #plot(1:2, type = 'n'); rasterImage(img, 1, 1, 2, 2)
    #plot(1:2, type = 'n'); rasterImage(ro.img, 1, 1, 2, 2)

    # Re-crop
    oldn <- nrow(img)
    newn <- nrow(ro.img)
    if (oldn != newn) {
      if (newn %% 2 == 1) { newn <- newn + 1 }
      frame <- ((newn - oldn) / 2):((newn - (newn - oldn) / 2) - 1)
      ro.img <- ro.img[frame, frame, ]
    }
    cr.img <- ro.img[lims[1]:lims[2], lims[1]:lims[2], ]

    # need <- (type[i] %in% c('E', 'D', 'S', 'B'))
    # ro.img <- sh.img
    # if (need == TRUE) {
    #   p1 <- sh.img[round(pix * ncol(sh.img)), round(pix * nrow(sh.img)), 1]
    #   p2 <- sh.img[round(pix * ncol(sh.img)), round((1 - pix) * nrow(sh.img)), 1]
    #   p3 <- sh.img[round((1 - pix) * ncol(sh.img)), round(pix * nrow(sh.img)), 1]
    #   p4 <- sh.img[round((1 - pix) * ncol(sh.img)), round((1 - pix) * nrow(sh.img)), 1]
    #   if ((p1 != 0 & p2 == 0) | (p3 == 0 & p4 != 0)) {
    #     ro.img[, , 1] <- pracma::rot90(sh.img[, , 1])
    #     ro.img[, , 2] <- pracma::rot90(sh.img[, , 2])
    #     ro.img[, , 3] <- pracma::rot90(sh.img[, , 3])
    #   }
    # }

    # 3. Sharpen
    sh.img <- cr.img
    sh.img[which(cr.img < min.int)] <- 0

    # 4. PCS
    pc.img <- sh.img
    svd1 <- svd(sh.img[, , 1])
    svd2 <- svd(sh.img[, , 2])
    svd3 <- svd(sh.img[, , 3])
    svd1[['d']][(pc.kept + 1):length(svd1[['d']])] <- 0
    svd2[['d']][(pc.kept + 1):length(svd2[['d']])] <- 0
    svd3[['d']][(pc.kept + 1):length(svd3[['d']])] <- 0
    pc.img[, , 1] <- svd1[['u']] %*% diag(svd1[['d']]) %*% t(svd1[['v']])
    pc.img[, , 2] <- svd2[['u']] %*% diag(svd2[['d']]) %*% t(svd2[['v']])
    pc.img[, , 3] <- svd3[['u']] %*% diag(svd3[['d']]) %*% t(svd3[['v']])
    pc.img[pc.img < 0] <- 0
    pc.img[pc.img > 1] <- 1
    #plot(1:2, type = 'n'); rasterImage(pc.img, 1, 1, 2, 2)

    # Save result
    out.file <- paste(DATDIR, 'img_crop/', img.list[i], sep = '')
    jpeg::writeJPEG(pc.img, target = out.file)

    # Dimensionality of X
    if (extensive == TRUE) {
      pc.vec <- c(as.vector((svd1[['u']] %*% diag(svd1[['d']]))[, 1:pc.kept]),
                  as.vector((svd2[['u']] %*% diag(svd2[['d']]))[, 1:pc.kept]),
                  as.vector((svd3[['u']] %*% diag(svd3[['d']]))[, 1:pc.kept]))
    } else {
      # l <- ncol(ro.img[, , 1])
      # ll <- 1:(l * 3)
      # s <- c()
      # for (pt in ll) { s <- c(s, seq(pt, length(ll), l)) }
      # mat <- cbind(ro.img[, , 1], ro.img[, , 2], ro.img[, , 3])[, s]
      mat <- cbind(sh.img[, , 1], sh.img[, , 2], sh.img[, , 3])
      dec <- try(svd(mat))
      if (class(dec) == 'try-error') {
        cat('\nError:', i, '\n')
        next
      }
      pc.vec <- as.vector((dec[['u']] %*% diag(dec[['d']]))[, 1:pc.kept])
    }

    # Percentage of explained variance
    evar[i] <- sum(diag(dec[['d']])[, 1:pc.kept]) / sum(diag(dec[['d']]))

    # Build matrix
    if (i == 1) {
      p <- length(pc.vec)
      X <- matrix(nrow = n, ncol = p)
      cat('* Dimension of X: n = ', n, '; p = ', p, '\n', sep = '')
    }
    X[i, ] <- pc.vec
  }; cat(' [Done]\n')

  # End
  return(invisible(list(X = X, evar = evar)))
}
# END OF SCRIPT

# par(mfrow = c(1, 2))
# plot(1:2, type='n'); rasterImage(ro.img,1,1,2,2)
# rasterImage(ro.img,1,1,2,2)
# plot(1:2, type='n')
# rasterImage(pc.img,1,1,2,2)

################################################################################
kfold <- function(n, k, seed = 666) {#, type = NULL) {
################################################################################
  folds <- vector(k, mode = 'list')
  if (! missing(seed)) {
    set.seed(seed)
    mix <- sample(1:n, n, replace = FALSE)
    nfolds <- n %/% k
    j <- 0
    for (i in 1:k) {
      folds[[i]] <- mix[(j + 1):(j + nfolds)]
      if (i == k) {
        folds[[i]] <- mix[(j + 1):(j + nfolds + (n %% k))]
      }
      folds[[i]] <- sort(folds[[i]])
      j <- j + nfolds
    }
  # } else {
  #   nfolds <- n %/% k
  #   types <- unique(as.character(type))
  #   nc <- length(types)
  #   pool <- vector(nc, mode = 'list')
  #   for (v in 1:nc) {
  #     pool[[v]] <- which(as.character(type) == types[v])
  #   }
  #   boxes <- sort(rep(1:k, nfolds))
  #   for (j in 1:n) {
  #     folds[[boxes[j]]] <- c(folds[[boxes[j]]], unlist(lapply(pool, '[[', j)))
  #   }

  #   for (i in 1:k) {

  #   }
  }
  return(folds)
}





