#' @export
stdSMOTE <- function(form,dat,perc.o,thr,k,calcPhi=TRUE) {

  require(UBL)

  y <- dat[,as.character(form[[2]])]

  pc <- IRon::phi.control(y)

  phis <- c()
  over.exs <- c()

  if(calcPhi) {
    phis <- IRon::phi(y,phi.parms = pc)

    over.exs <- dat[phis>=thr,]
  } else {
    over.exs <- dat
  }

  if(any(sapply(dat,is.numeric)==FALSE)){
    new.over.exs <- Smote.exsRegress(over.exs,which(colnames(dat)==as.character(form[[2]])),perc.o,k,"HEOM",2)
  } else {
    new.over.exs <- Smote.exsRegress(over.exs,which(colnames(dat)==as.character(form[[2]])),perc.o,k,"Euclidean",2)
  }


  new.dat <- rbind(dat,new.over.exs)

  tibble(new.dat)

}

# ===================================================
# Obtain a set of smoted examples for a set of rare cases.
#
# L. Torgo, Jun 2008
# P.Branco, Mar 2015 Apr 2016
# ---------------------------------------------------
Smote.exsRegress <- function(dat, tgt, N, k, dist, p)
  # INPUTS:
  # dat are the rare cases (the minority "class" cases)
  # tgt the column nr of the target variable
  # N is the percentage of over-sampling to carry out;
  # and k is the number of nearest neighours
  # dist is the distance function used for the neighours computation
  # p is an integer used when a "p-norm" distance is selected
  # OUTPUTS:
  # The result of the function is a (N-1)*nrow(dat) set of generate
  # examples with rare values on the target
{
  # check for constant features and remove them, if any
  # add the constant value of those features in the returned synthetic examples

  ConstFeat <- which(apply(dat, 2, function(col){length(unique(col)) == 1}))

  if(length(ConstFeat)){
    badds <- dat
    ConstRes <- dat[1,ConstFeat]
    dat <- dat[,apply(dat, 2, function(col) { length(unique(col)) > 1 })]
    tgt <- ncol(dat)
  }

  nomatr <- c()
  T <- matrix(nrow = dim(dat)[1], ncol = dim(dat)[2])
  for (col in seq.int(dim(T)[2])){
    if (class(dat[, col]) %in% c('factor', 'character')) {
      T[, col] <- as.integer(dat[, col])
      nomatr <- c(nomatr, col)
    } else {
      T[, col] <- dat[, col]
    }
  }
  nC <- dim(T)[2]
  nT <- dim(T)[1]


  ranges <- rep(1, nC)
  if (length(nomatr)) {
    for (x in (1:nC)[-c(nomatr)]) {
      ranges[x] <- max(T[, x]) - min(T[, x])
    }
  } else {
    for(x in (1:nC)) {
      ranges[x] <- max(T[, x]) - min(T[, x])
    }
  }

  kNNs <- neighbours(tgt, dat, dist, p, k)

  nexs <- as.integer(N - 1) # nr of examples to generate for each rare case
  extra <- as.integer(nT * (N - 1 - nexs)) # the extra examples to generate
  idx <- sample(1:nT, extra)
  newM <- matrix(nrow = nexs * nT + extra, ncol = nC)    # the new cases

  if (nexs) {
    for (i in 1:nT) {
      for (n in 1:nexs) {
        # select randomly one of the k NNs
        neig <- sample(1:k, 1)
        # the attribute values of the generated case
        difs <- T[kNNs[i, neig], -tgt] - T[i, -tgt]
        newM[(i - 1) * nexs + n, -tgt] <- T[i, -tgt] + runif(1) * difs
        for (a in nomatr) {
          # nominal attributes are randomly selected among the existing
          # values of seed and the selected neighbour
          newM[(i - 1) * nexs + n, a] <- c(T[kNNs[i, neig], a],
            T[i, a])[1 + round(runif(1), 0)]
        }
        # now the target value (weighted (by inverse distance) average)
        d1 <- d2 <- 0
        for (x in (1:nC)[-c(nomatr, tgt)]) {
          d1 <- abs(T[i, x] - newM[(i - 1) * nexs + n, x])/ranges[x]
          d2 <- abs(T[kNNs[i, neig], x] - newM[(i - 1) * nexs + n, x])/ranges[x]
        }
        if (length(nomatr)) {
          d1 <- d1 + sum(T[i, nomatr] != newM[(i - 1) * nexs + n, nomatr])
          d2 <- d2 +
            sum(T[kNNs[i, neig], nomatr] != newM[(i - 1) * nexs + n, nomatr])
        }
        # (d2+d1-d1 = d2 and d2+d1-d2 = d1) the more distant the less weight
        if (d1 == d2) {
          newM[(i - 1) * nexs + n, tgt] <- (T[i, tgt] + T[kNNs[i, neig], tgt])/2
        } else {
          newM[(i - 1) * nexs + n, tgt] <- (d2 * T[i, tgt] +
              d1 * T[kNNs[i, neig], tgt])/(d1 + d2)
        }
      }
    }
  }

  if (extra) {
    count <- 1
    for (i in idx) {
      # select randomly one of the k NNs
      neig <- sample(1:k, 1)

      # the attribute values of the generated case
      difs <- T[kNNs[i, neig], -tgt] - T[i, -tgt]
      newM[nexs * nT + count, -tgt] <- T[i, -tgt] + runif(1) * difs
      for (a in nomatr) {
        newM[nexs * nT + count, a] <- c(T[kNNs[i,neig], a],
          T[i, a])[1 + round(runif(1), 0)]
      }

      # now the target value (weighted (by inverse distance) average)
      d1 <- d2 <- 0
      for (x in (1:nC)[-c(nomatr,tgt)]) {
        d1 <- abs(T[i, x] - newM[nexs * nT + count, x])/ranges[x]
        d2 <- abs(T[kNNs[i, neig], x] - newM[nexs * nT + count, x])/ranges[x]
      }
      if (length(nomatr)) {
        d1 <- d1 + sum(T[i,nomatr] != newM[nexs *nT + count, nomatr])
        d2 <- d2 +
          sum(T[kNNs[i, neig], nomatr] != newM[nexs * nT + count, nomatr])
      }
      # (d2+d1-d1 = d2 and d2+d1-d2 = d1) the more distant the less weight
      if (d1 == d2) {
        newM[nexs * nT + count, tgt] <- (T[i, tgt] + T[kNNs[i, neig], tgt])/2
      } else {
        newM[nexs * nT + count, tgt] <- (d2 * T[i, tgt] +
            d1 * T[kNNs[i, neig],tgt])/(d1 + d2)
      }

      count <- count + 1
    }
  }

  newCases <- data.frame(newM)
  for (a in nomatr) {
    newCases[, a] <- factor(newCases[, a],
      levels = 1:nlevels(dat[, a]),
      labels = levels(dat[, a]))
  }

  if(length(ConstFeat)){ # add constant features that were removed in the beginning

    newCases <- cbind(newCases,
      as.data.frame(lapply(ConstRes,
        function(x){rep(x, nrow(newCases))})))
    colnames(newCases) <- c(colnames(dat), names(ConstFeat))
    newCases <- newCases[colnames(badds)]

  } else {
    colnames(newCases) <- colnames(dat)
  }

  newCases

}
