createPDs<-function(holdout){
  load("ProjectModel.RData")
  holdout$Default.Date <- as.Date(holdout$Default.Date,"%m/%d/%Y")
  days.to.default <- holdout$Default.Date - holdout$repdte
  default.in.cur.year<-days.to.default >0 & days.to.default <= 365.25
  holdout$default.flag <- ifelse(default.in.cur.year,1,0)
  holdout$default.flag <- ifelse(is.na(holdout$default.flag), 0, holdout$default.flag)
  cat("There are", sum(holdout$default.flag), "defaults in total")
  holdout.y <- holdout$default.flag
  holdout$default.flag <- NULL
  
  preprocess <- function(df, features.to.keep, kept.feature.means, features.to.use, feature.bounds) {
    # dropping features that have too many na's
    df <- df[, features.to.keep]
    # filling the remaining na's using average values
    na.counts <- sapply(df, function(x) sum(is.na(x)))
    features.with.na <- names(na.counts[na.counts>0])
    # filling in the remaining ones using overall average
    for (col in features.with.na) {
      df[[col]][is.na(df[[col]])] <- kept.feature.means[[col]]
    }
    # generating new features
    df$eqtot[df$eqtot < 1] <- 1
    df$lever.ratio <- df$liab / df$eqtot
    df$eintexp[df$eintexp < 1] <- 1
    df$debt.cover.ratio <- df$noij / df$eintexp
    df$liab[df$liab < 1] <- 1
    df$curr.ratio <- df$asset/df$liab
    # selecting features to use
    df <- df[, features.to.use]
    # mapping each value to percentile using the feature.bounds lookup table
    for (col in colnames(df)) {
      print(col)
      emp.dist <- ecdf(df[,col])
      df[,col] <- emp.dist(df[,col])
#       last.bound <- -Inf
#       for (row in rownames(feature.bounds)) {
#         mask <- (df[[col]] >= last.bound) & (df[[col]] < feature.bounds[row, col])
#         df[[col]][mask] <- as.numeric(row)/nrow(feature.bounds)
#         last.bound <- feature.bounds[row, col]
#       }
#       df[[col]][df[[col]] >= last.bound] <- 1.
    }
    return(df)
  }
  
  holdout.perc <- preprocess(holdout, features.to.keep, kept.feature.means, features.to.use, feature.bounds)
  holdout.perc$default.flag <- holdout.y
  
  out.pred2 <- predict(sweet.model, newdata = holdout.perc, type="response")
  
  nondefault.records.o <- holdout.perc$default.flag==0 #actual nondefaults
  default.records.o <- holdout.perc$default.flag==1 #actual defaults
  n.ndef.o <- sum(nondefault.records.o, na.rm=TRUE)
  n.def.o <- sum(default.records.o, na.rm=TRUE)
  ndef.PDs.o <- out.pred2[nondefault.records.o] #model estimates for non-defaulted firms
  def.PDs.o <- out.pred2[default.records.o] #model estimates for defaulted firms
  W.o <- wilcox.test(x=def.PDs.o, y=ndef.PDs.o, paired=FALSE)
  aprox.AUC.o <- W.o$statistic/(n.ndef.o*n.def.o)
  print(aprox.AUC.o)
}

setwd("~/Development/credit_modeling/proj/")
load("bankdata.holdout.RData")
holdout.pred <- createPDs(bankdata.holdout)
save(holdout.pred, file="holdout.pred.RData")