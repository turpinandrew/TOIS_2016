#######
#
# WARNING!!!! Not quite right, need to do for topic-doc pairs, since a doc can appear for
# more than 1 topic.....
#
#


### magnitude estimation data

me <- read.table("data.txt", header=T, as.is=c(6:13))
topics <- unique(me$Topic)
docs <- unique(c(me[,5],me[,6],me[,7],me[,8],me[,9],me[,10],me[,11],me[,12]))



### sormunen judgements

sorm <- read.table("~/Dropbox/MagnitudeEstimationExperiments/sormunen/sormunen_qrels", col.names=c("Topic","Junk","Doc","Srel"), as.is=c(3))
sorm.t <- sorm[sorm$Topic %in% topics,]
sorm.t.d <- sorm.t[sorm.t$Doc %in% docs,]


### trec judgements

trec <- read.table("~/Dropbox/MagnitudeEstimationExperiments/sormunen/qrels.401-450.trec8.adhoc", col.names=c("Topic","Junk","Doc","Trel"), as.is=c(3))
trec.t <- trec[trec$Topic %in% topics,]
trec.t.d <- trec.t[trec.t$Doc %in% docs,]


### 
length(unique(docs))
# [1] 3922
length(unique(trec.t.d$Doc))
# [1] 3579
length(unique(sorm.t.d$Doc))
# [1] 857


