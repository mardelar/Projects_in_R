#####  Load Data  #####

install.packages("DMwR2")
install.packages("forcats")
install.packages("performanceEstimation")

library(ggplot2)
library(dplyr)
library(DMwR2)

data(sales, package="DMwR2")

sales

summary(sales)

nlevels(sales$ID)

nlevels(sales$Prod)

filter(sales,is.na(Quant),is.na(Val))

table(sales$Insp)/nrow(sales) * 100

ggplot(group_by(sales,ID) %>% summarize(nTrans=n()),aes(x=ID,y=nTrans)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_blank(), axis.ticks.x=element_blank()) + 
  xlab("Salesmen") + ylab("Nr. of Transactions") +
  ggtitle("Nr. of Transactions per Salesman")

ggplot(group_by(sales,Prod) %>% summarize(nTrans=n()),aes(x=Prod,y=nTrans)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_blank(), axis.ticks.x=element_blank()) + 
  xlab("Product") + ylab("Nr. of Transactions") +
  ggtitle("Nr. of Transactions per Product")

sales <- mutate(sales,Uprice=Val/Quant)

summary(sales$Uprice)

prods <- group_by(sales,Prod)

mpProds <- summarize(prods,medianPrice=median(Uprice,na.rm=TRUE))

bind_cols(mpProds %>% arrange(medianPrice) %>% slice(1:5),
          mpProds %>% arrange(desc(medianPrice)) %>% slice(1:5))

library(ggplot2)

library(forcats)

ggplot(filter(sales,Prod %in% c("p3689","p560")),aes(x=fct_drop(Prod),y=Uprice)) +
  geom_boxplot() + scale_y_log10() + 
  xlab("") + ylab("log10(UnitPrice)")

ids <- group_by(sales,ID)

tvIDs <- summarize(ids,totalVal=sum(Val,na.rm=TRUE))

bind_cols(tvIDs %>% arrange(totalVal) %>% slice(1:5),
          tvIDs %>% arrange(desc(totalVal)) %>% slice(1:5))

arrange(tvIDs,desc(totalVal)) %>% slice(1:100) %>% 
  summarize(t100=sum(totalVal)) / 
  (summarize(tvIDs,sum(totalVal))) * 100

arrange(tvIDs,totalVal) %>% slice(1:2000) %>% 
  summarize(b2000=sum(totalVal)) / 
  (summarize(tvIDs,sum(totalVal))) * 100

prods <- group_by(sales,Prod)

qtProds <- summarize(prods,totalQty=sum(Quant,na.rm=TRUE))

bind_cols(qtProds %>% arrange(desc(totalQty)) %>% slice(1:5),
          qtProds %>% arrange(totalQty) %>% slice(1:5))

arrange(qtProds,desc(totalQty)) %>% slice(1:100) %>% 
  summarize(t100=sum(as.numeric(totalQty))) / 
  (summarize(qtProds,sum(as.numeric(totalQty)))) * 100

arrange(qtProds,totalQty) %>% slice(1:4000) %>% 
  summarize(b4000=sum(as.numeric(totalQty))) / 
  (summarize(qtProds,sum(as.numeric(totalQty)))) * 100


nouts <- function(x) length(boxplot.stats(x)$out)

noutsProds <- summarise(prods,nOut=nouts(Uprice))

arrange(noutsProds,desc(nOut))

summarize(noutsProds,totalOuts=sum(nOut))

summarize(noutsProds,totalOuts=sum(nOut))/nrow(sales)*100



#####  Data Problems  #####



prop.naQandV <- function(q,v) 100*sum(is.na(q) & is.na(v))/length(q)

summarise(ids,nProbs=prop.naQandV(Quant,Val)) %>% arrange(desc(nProbs))

summarise(prods,nProbs=prop.naQandV(Quant,Val)) %>% arrange(desc(nProbs))

sales <- filter(sales,!(is.na(Quant) & is.na(Val)))

prop.nas <- function(x) 100*sum(is.na(x))/length(x)

summarise(prods,propNA.Q=prop.nas(Quant)) %>% arrange(desc(propNA.Q))

filter(sales, Prod %in% c("p2442","p2443")) %>% 
  group_by(Insp) %>% count()

sales <- droplevels(filter(sales,!(Prod %in% c("p2442", "p2443"))))

summarise(ids,propNA.Q=prop.nas(Quant)) %>% arrange(desc(propNA.Q))

summarise(prods,propNA.V=prop.nas(Val)) %>% arrange(desc(propNA.V))

summarise(ids,propNA.V=prop.nas(Val)) %>% arrange(desc(propNA.V))

tPrice <- filter(sales, Insp != "fraud") %>% 
  group_by(Prod) %>% 
  summarise(medianPrice = median(Uprice,na.rm=TRUE))

noQuantMedPrices <- filter(sales, is.na(Quant)) %>% 
  inner_join(tPrice) %>% 
  select(medianPrice)
noValMedPrices <- filter(sales, is.na(Val)) %>% 
  inner_join(tPrice) %>% 
  select(medianPrice)

noQuant <- which(is.na(sales$Quant))
noVal <- which(is.na(sales$Val))
sales[noQuant,'Quant'] <- ceiling(sales[noQuant,'Val'] /noQuantMedPrices)
sales[noVal,'Val'] <- sales[noVal,'Quant'] * noValMedPrices

sales$Uprice <- sales$Val/sales$Quant

save(sales, file = "salesClean.Rdata")


#####  Few Transactions of Some Products  #####


ms <- filter(sales,Insp != "fraud") %>% 
  group_by(Prod) %>% 
  summarize(median=median(Uprice,na.rm=TRUE),
            iqr=IQR(Uprice,na.rm=TRUE),
            nTrans=n(),
            fewTrans=ifelse(nTrans>20,FALSE,TRUE))
ms

ggplot(ms,aes(x=median,y=iqr,color=fewTrans)) + 
  geom_point() + 
  xlab("Median") + ylab("IQR")

ggplot(ms,aes(x=median,y=iqr,color=fewTrans)) + 
  geom_point() + 
  scale_y_log10() + scale_x_log10() + 
  xlab("log(Median)") + ylab("log(IQR)")

ms <- mutate(ms,smedian=scale(median),siqr=scale(iqr))

smalls <- which(ms$fewTrans)

nsmalls <- as.character(ms$Prod[smalls])

similar <- matrix(NA,length(smalls),7,
                  dimnames=list(nsmalls,
                                c("RowSimProd", "ks.stat", "ks.p", "medP", "iqrP", "medS","iqrS")))

xprods <- tapply(sales$Uprice, sales$Prod, list)

for(i in seq_along(smalls)) {
  d <- scale(ms[,c("smedian","siqr")],
             c(ms$smedian[smalls[i]],ms$siqr[smalls[i]]),
             FALSE)
  d <- sqrt(drop(d^2 %*% rep(1, ncol(d))))
  stat <- ks.test(xprods[[nsmalls[i]]], xprods[[order(d)[2]]])
  similar[i, ] <- c(order(d)[2], stat$statistic, stat$p.value,
                    ms$median[smalls[i]],ms$iqr[smalls[i]],
                    ms$median[order(d)[2]],ms$iqr[order(d)[2]])
}

head(similar)

bind_rows(filter(ms,Prod==rownames(similar)[1]),
          ms[similar[1,1],])

nrow(similar[similar[, "ks.p"] >= 0.9, ])

sum(similar[, "ks.p"] >= 0.9)

save(similar, file = "similarProducts.Rdata")

library(ROCR)
data(ROCR.simple)
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred, "prec", "rec")
plot(perf)

PRcurve <- function(preds, trues, ...) {
  require(ROCR, quietly = TRUE)
  pd <- prediction(preds, trues)
  pf <- performance(pd, "prec", "rec")
  pf@y.values <- lapply(pf@y.values, function(x) rev(cummax(rev(x))))
  plot(pf, ...)
}

PRcurve(ROCR.simple$predictions, ROCR.simple$labels)

library(ROCR)
data(ROCR.simple)
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred, "prec", "rec")
par( mfrow=c(1,2) )
plot(perf)
PRcurve(ROCR.simple$predictions, ROCR.simple$labels)
par( mfrow=c(1,1) )

pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred, "lift", "rpp")
plot(perf, main = "Lift Chart")

CRchart <- function(preds, trues, ...) {
  require(ROCR, quietly = T)
  pd <- prediction(preds, trues)
  pf <- performance(pd, "rec", "rpp")
  plot(pf, ...)
}

CRchart(ROCR.simple$predictions, ROCR.simple$labels, 
        main='Cumulative Recall Chart')

avgNDTP <- function(toInsp,train,stats) {
  if (missing(train) && missing(stats)) 
    stop('Provide either the training data or the product stats')
  if (missing(stats)) {
    stats <- as.matrix(filter(train,Insp != 'fraud') %>%
                         group_by(Prod) %>%
                         summarise(median=median(Uprice),iqr=IQR(Uprice)) %>%
                         select(median,iqr))
    rownames(stats) <- levels(train$Prod)
    stats[which(stats[,'iqr']==0),'iqr'] <- stats[which(stats[,'iqr']==0),'median']
  }
  
  return(mean(abs(toInsp$Uprice-stats[toInsp$Prod,'median']) /
                stats[toInsp$Prod,'iqr']))
}

evalOutlierRanking <- function(testSet,rankOrder,Threshold,statsProds,...) 
{
  ordTS <- testSet[rankOrder,]
  N <- nrow(testSet)
  nF <- if (Threshold < 1) as.integer(Threshold*N) else Threshold
  cm <- table(c(rep('fraud',nF),rep('ok',N-nF)),ordTS$Insp)
  prec <- cm['fraud','fraud']/sum(cm['fraud',])
  rec <- cm['fraud','fraud']/sum(cm[,'fraud'])
  AVGndtp <- avgNDTP(ordTS[1:nF,],stats=statsProds)
  return(c(Precision=prec,Recall=rec,avgNDTP=AVGndtp))
}

BPrule.wf <- function(form,train,test,...) {
  require(dplyr, quietly=TRUE)
  ms <- as.matrix(filter(train,Insp != 'fraud') %>%
                    group_by(Prod) %>%
                    summarise(median=median(Uprice),iqr=IQR(Uprice)) %>%
                    select(median,iqr))
  rownames(ms) <- levels(train$Prod)
  ms[which(ms[,'iqr']==0),'iqr'] <- ms[which(ms[,'iqr']==0),'median']
  ORscore <- abs(test$Uprice-ms[test$Prod,'median']) /
    ms[test$Prod,'iqr']
  rankOrder <- order(ORscore,decreasing=TRUE)
  res <- list(testSet=test,rankOrder=rankOrder,
              probs=matrix(c(ORscore,ifelse(test$Insp=='fraud',1,0)),
                           ncol=2))
  res
}

library(dplyr)
globalStats <- as.matrix(filter(sales,Insp != 'fraud') %>%
                           group_by(Prod) %>%
                           summarise(median=median(Uprice),iqr=IQR(Uprice)) %>%
                           select(median,iqr))
rownames(globalStats) <- levels(sales$Prod)
globalStats[which(globalStats[,'iqr']==0),'iqr'] <- 
  globalStats[which(globalStats[,'iqr']==0),'median']
head(globalStats,3)


library(performanceEstimation)
bp.res <- performanceEstimation(
  PredTask(Insp ~ ., sales),
  Workflow("BPrule.wf"),
  EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                 method=Holdout(nReps=3, hldSz=0.3, strat=TRUE),
                 evaluator="evalOutlierRanking",
                 evaluator.pars=list(Threshold=0.1, statsProds=globalStats))
)

summary(bp.res)

par(mfrow=c(1,2)) 
ps.bp <- sapply(getIterationsInfo(bp.res), function(i) i$probs[,1])
ts.bp <- sapply(getIterationsInfo(bp.res), function(i) i$probs[,2])
PRcurve(ps.bp, ts.bp, main="PR curve", avg="vertical")
CRchart(ps.bp, ts.bp, main='Cumulative Recall curve', avg='vertical')


LOF.wf <- function(form, train, test, k, ...) {
  require(DMwR2, quietly=TRUE)
  ntr <- nrow(train)
  all <- as.data.frame(rbind(train,test))
  N <- nrow(all)
  ups <- split(all$Uprice,all$Prod)
  r <- list(length=ups)
  for(u in seq(along=ups)) 
    r[[u]] <- if (NROW(ups[[u]]) > 3) 
      lofactor(ups[[u]],min(k,NROW(ups[[u]]) %/% 2)) 
  else if (NROW(ups[[u]])) rep(0,NROW(ups[[u]])) 
  else NULL
  all$lof <- vector(length=N)
  split(all$lof,all$Prod) <- r
  all$lof[which(!(is.infinite(all$lof) | is.nan(all$lof)))] <- 
    SoftMax(all$lof[which(!(is.infinite(all$lof) | is.nan(all$lof)))])
  
  res <- list(testSet=test,
              rankOrder=order(all[(ntr+1):N,'lof'],decreasing=TRUE),
              probs=as.matrix(cbind(all[(ntr+1):N,'lof'],
                                    ifelse(test$Insp=='fraud',1,0))))
  res
}

lof.res <- performanceEstimation(
  PredTask(Insp ~ . , sales),
  Workflow("LOF.wf", k=7),
  EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                 method=Holdout(nReps=3, hldSz=0.3, strat=TRUE),
                 evaluator="evalOutlierRanking",
                 evaluator.pars=list(Threshold=0.1, statsProds=globalStats))
)

summary(lof.res)

ps.lof <- sapply(getIterationsInfo(lof.res), function(i) i$probs[,1])
ts.lof <- sapply(getIterationsInfo(lof.res), function(i) i$probs[,2])
PRcurve(ps.bp,ts.bp,main="PR curve",lty=1,
        xlim=c(0,1),ylim=c(0,1),avg="vertical")
PRcurve(ps.lof,ts.lof,add=TRUE,lty=2,avg='vertical')
legend('topright',c('BPrule','LOF'),lty=c(1,2))

CRchart(ps.bp,ts.bp,main='Cumulative Recall curve',
        lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.lof,ts.lof,add=TRUE,lty=2,avg='vertical')
legend('bottomright',c('BPrule','LOF'),lty=c(1,2))

par(mfrow=c(1,2)) 
ps.lof <- sapply(getIterationsInfo(lof.res), function(i) i$probs[,1])
ts.lof <- sapply(getIterationsInfo(lof.res), function(i) i$probs[,2])
PRcurve(ps.bp, ts.bp,main="PR curve",lty=1,
        xlim=c(0,1),ylim=c(0,1),avg="vertical")
PRcurve(ps.lof, ts.lof,add=T,lty=2,avg='vertical')
legend('topright',c('BPrule','LOF'),lty=c(1,2))
CRchart(ps.bp,ts.bp,main='Cumulative Recall curve',
        lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.lof,ts.lof,add=T,lty=2,avg='vertical')
legend('bottomright',c('BPrule','LOF'),lty=c(1,2))

#####  Cluster Based Outlier Rankings  #####

ORh.wf <- function(form, train, test, ...) {
  require(DMwR2, quietly=TRUE)
  ntr <- nrow(train)
  all <- as.data.frame(rbind(train,test))
  N <- nrow(all)
  ups <- split(all$Uprice,all$Prod)
  r <- list(length=ups)
  for(u in seq(along=ups)) 
    r[[u]] <- if (NROW(ups[[u]]) > 3) 
      outliers.ranking(ups[[u]])$prob.outliers
  else if (NROW(ups[[u]])) rep(0,NROW(ups[[u]])) 
  else NULL
  all$orh <- vector(length=N)
  split(all$orh,all$Prod) <- r
  all$orh[which(!(is.infinite(all$orh) | is.nan(all$orh)))] <- 
    SoftMax(all$orh[which(!(is.infinite(all$orh) | is.nan(all$orh)))])
  res <- list(testSet=test,
              rankOrder=order(all[(ntr+1):N,'orh'],decreasing=TRUE),
              probs=as.matrix(cbind(all[(ntr+1):N,'orh'],
                                    ifelse(test$Insp=='fraud',1,0))))
  res
  
}

orh.res <- performanceEstimation(
  PredTask(Insp ~ . , sales),
  Workflow("ORh.wf"),
  EstimationTask(metrics=c("Precision","Recall","avgNDTP"),
                 method=Holdout(nReps=3, hldSz=0.3, strat=TRUE),
                 evaluator="evalOutlierRanking",
                 evaluator.pars=list(Threshold=0.1, statsProds=globalStats))
)

summary(orh.res)

ps.orh <- sapply(getIterationsInfo(orh.res), function(i) i$probs[,1])
ts.orh <- sapply(getIterationsInfo(orh.res), function(i) i$probs[,2])
PRcurve(ps.bp,ts.bp,main="PR curve",lty=1,
        xlim=c(0,1),ylim=c(0,1),avg="vertical")
PRcurve(ps.lof,ts.lof,add=TRUE,lty=2,avg='vertical')
PRcurve(ps.orh,ts.orh,add=TRUE,lty=1,col='grey', avg='vertical')
legend('topright',c('BPrule','LOF','ORh'),lty=c(1,2,1),
       col=c('black','black','grey'))

CRchart(ps.bp,ts.bp,main='Cumulative Recall curve',
        lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.lof,ts.lof,add=TRUE,lty=2,avg='vertical')
CRchart(ps.orh,ts.orh,add=TRUE,lty=1,col='grey',avg='vertical')
legend('bottomright',c('BPrule','LOF','ORh'),lty=c(1,2,1),
       col=c('black','black','grey'))

par(mfrow=c(1,2)) 
ps.orh <- sapply(getIterationsInfo(orh.res), function(i) i$probs[,1])
ts.orh <- sapply(getIterationsInfo(orh.res), function(i) i$probs[,2])
PRcurve(ps.bp,ts.bp,main="PR curve",lty=1,
        xlim=c(0,1),ylim=c(0,1),avg="vertical")
PRcurve(ps.lof,ts.lof,add=T,lty=2,avg='vertical')
PRcurve(ps.orh,ts.orh,add=T,lty=1,col='grey', avg='vertical')
legend('topright',c('BPrule','LOF','ORh'),lty=c(1,2,1),
       col=c('black','black','grey'))

CRchart(ps.bp,ts.bp,main='Cumulative Recall curve',
        lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(ps.lof,ts.lof,add=T,lty=2,avg='vertical')
CRchart(ps.orh,ts.orh,add=T,lty=1,col='grey',avg='vertical')
legend('bottomright',c('BPrule','LOF','ORh'),lty=c(1,2,1),
       col=c('black','black','grey'))




















