rm(list = ls())
setwd("~/Development/credit_modeling/proj/")
load("bankdata.in.new.RData")
cat("There are", nrow(bankdata.in.new), "data points in total")

## TRAIN VALIDATION SPLIT

# generating default flag for all data points
bankdata.in.new$Default.Date <- as.Date(bankdata.in.new$Default.Date,"%m/%d/%Y")
days.to.default <- bankdata.in.new$Default.Date - bankdata.in.new$repdte
default.in.cur.year<-days.to.default >0 & days.to.default <= 365.25
bankdata.in.new$default.flag <- ifelse(default.in.cur.year,1,0)
bankdata.in.new$default.flag <- ifelse(is.na(bankdata.in.new$default.flag), 0, bankdata.in.new$default.flag)
cat("There are", sum(bankdata.in.new$default.flag), "defaults in total")
bankdata.in.new$Default.Date <- NULL

# parsing year and month from date and generating time index
parse.year.from.date <- function(x) as.numeric(format(x,'%Y'))
bankdata.in.new$year <- parse.year.from.date(bankdata.in.new$repdte)
parse.month.from.date <- function(x) as.numeric(format(x,'%m'))
bankdata.in.new$month <- parse.month.from.date(bankdata.in.new$repdte)
unique(bankdata.in.new$month)
bankdata.in.new$time.index <- 10*bankdata.in.new$year+bankdata.in.new$month
bankdata.in.new$year <- NULL
bankdata.in.new$month <- NULL

# splitting the data by holding out 10% of the most recent default cases
split.based.on.default.thresh <- function(overall.thresh, default.thresh, df) {
  time.index.to.split.on <- quantile(df$time.index, overall.thresh)
  num.of.defaults <- sum(bankdata.in.new[df$time.index<time.index.to.split.on, ]$default.flag)
  if (num.of.defaults<default.thresh*sum(df$default.flag)) {
    split.based.on.default.thresh(overall.thresh+.01, default.thresh, df)
  }
  else {
    train <- bankdata.in.new[df$time.index<time.index.to.split.on, ]
    valid <- bankdata.in.new[df$time.index>=time.index.to.split.on, ]
    return(list(train, valid))
  }
}

split.output <- split.based.on.default.thresh(.9, .9, bankdata.in.new)
train <- split.output[[1]]
valid <- split.output[[2]]
train.y <- train$default.flag
valid.y <- valid$default.flag
train$ID <- NULL
train$repdte <- NULL
train$default.flag <- NULL
train$time.index <- NULL
valid$time.index <- NULL
valid$default.flag <- NULL


## FEATURE ENGINEERING using in-sample data

# dropping all features that have more than 10% data missing
na.counts <- sapply(train, function(x) sum(is.na(x)))
thresh <- nrow(train)/10
cat("After getting rid of the features that have more than 10% of data missing, we have",length(na.counts[na.counts<thresh]),"features, instead of",ncol(bankdata.in.new)-1)
features.to.keep <- names(na.counts[na.counts<=thresh])
features.to.drop <- names(na.counts[na.counts>thresh])
train <- train[, features.to.keep]
cat("Dropped features include:", features.to.drop)

# getting all features that have na's
na.counts <- sapply(train, function(x) sum(is.na(x)))
features.with.na <- names(na.counts[na.counts>0])

# filling in the remaining ones using overall average
for (col in features.with.na) {
  train[[col]][is.na(train[[col]])] <- mean(train[[col]], na.rm = TRUE)
}
# saving feature means to later preprocess validation / testing data
kept.feature.means <- colMeans(train[,features.to.keep], na.rm = TRUE)

# VARIABLE SELECTION BY BROAD CATEGORY

# PROFITABILITY - checking out profitability ratios (roe, roa) and efficiency ratio (eeffr)
summary(train$roe)
summary(train$roa)
summary(train$eeffr)

# LEVERAGE - adding in leverage ratio: liability / equity
train$eqtot[train$eqtot < 1] <- 1
train$lever.ratio <- train$liab / train$eqtot
summary(train$lever.ratio)

# DEBT COVERAGE - adding in debt coverage ratio: cash flow / interest payment
train$eintexp[train$eintexp < 1] <- 1
train$debt.cover.ratio <- train$noij / train$eintexp
summary(train$debt.cover.ratio)

# LIQUIDITY - adding in current ratio: asset / liability
train$liab[train$liab < 1] <- 1
train$curr.ratio <- train$asset/train$liab
summary(train$curr.ratio)

# GROWTH - change of percentage in assets
# train$asset.growth <- train$asset
# for (id in unique(train$ID)) {
#   temp <- train[train$ID==id, "asset"]
#   if (length(temp)==1) {
#     train[train$ID==id, "asset.growth"] <- 0.
#   }
#   else {
#     train[train$ID==id, "asset.growth"] <- append(temp[1:(length(temp)-1)], temp[1], after = 0)
#     train[train$ID==id, "asset.growth"] <- 100*(temp - train[train$ID==id, "asset.growth"]) / temp
#     train[train$ID==id, "asset.growth"][1] <- train[train$ID==id, "asset.growth"][2]
#   }
# }
# train$asset.growth <- NULL

# ACTIVITY - inventory and sales don't matter much to banks

# SIZE - checking out total asset
summary(bankdata.in.new$asset)

# CAPITAL
summary(bankdata.in.new$RBCT1J)
summary(bankdata.in.new$rbc1aaj)

# constructing correlation matrix
cor.mat <- cor(as.matrix(train), use="complete.obs", method="pearson")
train.baseline <- subset(train, select = c(roe, roa, eeffr, lever.ratio, debt.cover.ratio, curr.ratio, asset, RBCT1J, rbc1aaj))

# dropping out all variables that have correlation with others above 0.80
tmp <- cor.mat
tmp[upper.tri(tmp)] <- 0
diag(tmp) <- 0
train.new <- train[, !apply(tmp, 2, function(x) any(x > 0.80))]

# adding back baseline features that have been dropped
for (col in colnames(train.baseline)) {
  if (!(col %in% colnames(train.new))) {
    train.new[[col]] <- train.baseline[[col]]
  }
}
features.to.use <- names(train.new)

## MODEL DEVELOPMENT

# mapping values to percentiles
perc.rank <- function(x) trunc(rank(x))/length(x)

map.values.to.percentiles <- function(feature, increment) {
  bounds <- quantile(feature, seq(from = increment, to = 1, by = increment))
  # bounds[[length(bounds)]] <- Inf
  percentiles <- round(perc.rank(feature), 4)
  return(list(percentiles, bounds))
}

train.perc <- train.new
feature.bounds <- data.frame(matrix(nrow = 10000, ncol = 0))

for (col in colnames(train.new)) {
  map.output <- map.values.to.percentiles(train.new[[col]], .0001)
  train.perc[[col]] <- map.output[[1]]
  feature.bounds[[col]] <- map.output[[2]]
}
train.perc$default.flag <- train.y

# MODEL 1 - using baseline features
baseline.model <- glm(default.flag~roe+roa+eeffr+lever.ratio+debt.cover.ratio+curr.ratio+asset+RBCT1J+rbc1aaj, family=binomial(link="logit"), data=train.perc)
summary(baseline.model)
round(summary(baseline.model$fitted.values),5)

nondefault.records <- baseline.model$y==0 #actual non-defaulted firms
default.records <- baseline.model$y==1 #actual defaults
n.ndef <- sum(nondefault.records, na.rm=TRUE)
n.def <- sum(default.records,na.rm=TRUE)

ndef.PDs <- baseline.model$fitted.values[nondefault.records] #model estimates for non-defaulted firms
def.PDs <- baseline.model$fitted.values[default.records] #model estimates for defaulted firms

W <- wilcox.test(x=def.PDs, y=ndef.PDs, paired=FALSE)
aprox.AUC <- W$statistic/(n.ndef*n.def)
aprox.AUC

# MODEL 2 - using all selected features
sweet.model <- glm(default.flag~., family=binomial(link="logit"), data=train.perc)
summary(sweet.model)
round(summary(sweet.model$fitted.values),5)

nondefault.records <- sweet.model$y==0 #actual non-defaulted firms
default.records <- sweet.model$y==1 #actual defaults
n.ndef <- sum(nondefault.records, na.rm=TRUE)
n.def <- sum(default.records,na.rm=TRUE)

ndef.PDs <- sweet.model$fitted.values[nondefault.records] #model estimates for non-defaulted firms
def.PDs <- sweet.model$fitted.values[default.records] #model estimates for defaulted firms

W <- wilcox.test(x=def.PDs, y=ndef.PDs, paired=FALSE)
aprox.AUC <- W$statistic/(n.ndef*n.def)
aprox.AUC


## VALIDATION - out sample test

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
#     last.bound <- -Inf
#     for (row in rownames(feature.bounds)) {
#       mask <- (df[[col]] >= last.bound) & (df[[col]] < feature.bounds[row, col])
#       df[[col]][mask] <- as.numeric(row)/nrow(feature.bounds)
#       last.bound <- feature.bounds[row, col]
#     }
#     df[[col]][df[[col]] >= last.bound] <- 1.
  }
  return(df)
}

valid.perc <- preprocess(valid, features.to.keep, kept.feature.means, features.to.use, feature.bounds)
valid.perc$default.flag <- valid.y

# MODEL 1
out.pred <- predict(baseline.model, newdata = valid.perc, type="response")
nondefault.records.o <- valid.perc$default.flag==0 #actual nondefaults
default.records.o <- valid.perc$default.flag==1 #actual defaults
n.ndef.o <- sum(nondefault.records.o, na.rm=TRUE)
n.def.o <- sum(default.records.o, na.rm=TRUE)
ndef.PDs.o <- out.pred[nondefault.records.o] #model estimates for non-defaulted firms
def.PDs.o <- out.pred[default.records.o] #model estimates for defaulted firms
W.o <- wilcox.test(x=def.PDs.o, y=ndef.PDs.o, paired=FALSE)
aprox.AUC.o <- W.o$statistic/(n.ndef.o*n.def.o)
aprox.AUC.o

colAUC(data.frame(out.pred), valid.perc$default.flag,plotROC=TRUE, alg=c("ROC"))

# MODEL 2
out.pred2 <- predict(sweet.model, newdata = valid.perc, type="response")
nondefault.records.o <- valid.perc$default.flag==0 #actual nondefaults
default.records.o <- valid.perc$default.flag==1 #actual defaults
n.ndef.o <- sum(nondefault.records.o, na.rm=TRUE)
n.def.o <- sum(default.records.o, na.rm=TRUE)
ndef.PDs.o <- out.pred2[nondefault.records.o] #model estimates for non-defaulted firms
def.PDs.o <- out.pred2[default.records.o] #model estimates for defaulted firms
W.o <- wilcox.test(x=def.PDs.o, y=ndef.PDs.o, paired=FALSE)
aprox.AUC.o <- W.o$statistic/(n.ndef.o*n.def.o)
aprox.AUC.o

library("caTools")
colAUC(data.frame(out.pred), valid.perc$default.flag,plotROC=TRUE, alg=c("ROC"))
#colAUC(data.frame(model=out.pred, liquidity=testdat.clean$wc.ta.adj), testdat.clean$default.flag, plotROC=TRUE, alg=c("ROC"))
colAUC(data.frame(model=out.pred, out.pred2), valid.perc$default.flag, plotROC=TRUE, alg=c("ROC"))
save(features.to.keep,kept.feature.means,features.to.use,feature.bounds,sweet.model, file = "ProjectModel.RData")
