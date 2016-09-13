#
# For all doc pairs (within a topic), is ME consistent with
# Sormunen or TREC? How compare to Sormunen vs TREC?
# Exclude pairs where S/T are equal (it is unlikely ME will be equal).
#
# Thu 17 Dec 2015 11:04:52 AEDT
# Andrew Turpin
#

require(parallel)

##########################################################
# Returns agreement per topic for category specified
#
# j1 in {"sorm", "trec"}
# cat1 and cat2 are two Sorm categories (0123)
##########################################################
agree <- function(cat1, cat2, j1) {
    l <- mclapply(t, function(ds) {
        if (is.na(cat1)) {
            z <- ds[,j1] != "N" 
        } else {
            z <- (ds$sorm == cat1 | ds$sorm == cat2) & ds[,j1] != "N" 
        }
        ds <- ds[z,]
        a <- sapply(1:(nrow(ds)-1), function(i) {
           for(j in (i+1):nrow(ds))
                if (ds[i,j1] == ds[j,j1]) {
                    return(NA)
                } else {
                    return(
                           (ds[i,j1] < ds[j,j1] && ds$norm[i] < ds$norm[j]) 
                        || (ds[i,j1] > ds[j,j1] && ds$norm[i] > ds$norm[j]) 
                    )
                }
        })
        z <- !is.na(a)
        if (sum(z)  > 0) {
            a <- a[z]
            return(a)
        } else {
            return(NA)
        }
    })
    return(mclapply(l, function(x) sum(x)/length(x)))
}

##########################################################
#
agree_T_S <- function(cat1, cat2) {
    l <- mclapply(t, function(ds) {
        if (is.na(cat1)) {
            z <- ds$sorm != "N" & ds$trec != "N" 
        } else {
            z <- (ds$sorm == cat1 | ds$sorm == cat2) & ds$sorm != "N" & ds$trec != "N" 
        }
        ds <- ds[z,]

        z <- rep(NA, nrow(ds))
        for (dd in unique(ds$document)) {
            ii <- which(ds$document == dd)
            z[ii] <- FALSE
            z[ii[1]] <- TRUE
        }
        ds <- ds[z,]
        
        a <- NULL
        for (i in 1:nrow(ds)) {
            z <- which(ds$sorm[i:nrow(ds)] != ds$sorm[i]) + i-1
            for(j in z) {
                a <- c(a, 
                      #     (ds$sorm[i] <= ds$sorm[j] && ds$trec[i] <= ds$trec[j]) 
                      #  || (ds$sorm[i] >  ds$sorm[j] && ds$trec[i] > ds$trec[j]) 

                           (ds$sorm[i] == 0 && ds$sorm[j] != 0 && ds$trec[i] == 0 && ds$trec[j] == 1) 
                        || (ds$sorm[i] != 0 && ds$sorm[j] == 0 && ds$trec[i] == 1 && ds$trec[j] == 0) 
                        || (ds$sorm[i] != 0 && ds$sorm[j] != 0 && ds$trec[i] == 1 && ds$trec[j] == 1) 

                      #     (ds$sorm[i] <= "1" && ds$sorm[j] > "1" && ds$trec[i] == "0" && ds$trec[j] == "1") 
                      #  || (ds$sorm[i] > "1" && ds$sorm[j] <= "1" && ds$trec[i] == "1" && ds$trec[j] == "0") 
                      #  || (ds$trec[i] == "1" && ds$trec[j] == "1") 
                )
            }
        }
        return(a)
    })
    #return(l)
    return(mclapply(l, function(x) sum(x)/length(x)))
}

##################################################################

###t <- mclapply(split(d, list(d$topic_id)), "[", c("document","norm", "sorm", "trec"))

###
###a_T_S <- vector("list")
###a_T_S[["NM"]]<- agree_T_S("0", "1")
###a_T_S[["NR"]]<- agree_T_S("0", "2")
###a_T_S[["NH"]]<- agree_T_S("0", "3")
###a_T_S[["MR"]]<- agree_T_S("1", "2")
###a_T_S[["MH"]]<- agree_T_S("1", "3")
###a_T_S[["RH"]]<- agree_T_S("2", "3")
###a_T_S[["All"]]<- agree_T_S(NA,NA)

###load(file="pairwise2.Rdata")
###rm("a_T_ME")
###    # a_S_ME  is list indexed by S-category pairs of agreement for each topic
###    # a_T_S   is list indexed by S-category pairs of agreement for each topic
###
###a_T_ME <- unlist(lapply(t, function(ds) {
###            z0 <- which(ds$trec == "0")
###            z1 <- which(ds$trec == "1")
###            ps <- expand.grid(z0, z1)
###            ags <- ds$norm[ps[,1]] < ds$norm[ps[,2]]
###            sum(ags)/nrow(ps)
###          }))
a_T_S <- unlist(lapply(t, function(ds) {
            z <- ds$sorm != "N" & ds$trec != "N" 
            ds <- ds[z,]

            z <- rep(NA, nrow(ds))
            for (dd in unique(ds$document)) {
                ii <- which(ds$document == dd)
                z[ii] <- FALSE
                z[ii[1]] <- TRUE
            }
            ds <- ds[z,]
        
            s <- ds$sorm
            s[ds$sorm == "0"] <- "0"
            s[ds$sorm != "0"] <- "1"

            ags <- ds$trec == s
            sum(ags)/nrow(ds)
          }))
##################################################################
options(errors=dev.off)
pdf("pairwise5.pdf", width=2.5*7/cm(1), height=1*7/cm(1))
par(mar=c(0,3,0,0)+0.5)
par(mgp=c(2.5,1,0))


plot(0,0,type="n", xlab="", 
    ylab="Doc-pairs agreeing on relevance order (%)", 
    xlim=c(0,10), ylim=c(20,100), axes=FALSE,
    cex.lab=0.8
)
box()
axis(2, las=1)

abline(v=c(6.5,8.5), lty=3, col=grey(0.6))

xpos <- 0
for (cc in c("NH", "NR", "MH", "RH", "MR", "NM", "All")) {
    points(rep(xpos, length(a_S_ME[[cc]])), 100*unlist(a_S_ME[[cc]]), pch=21,col=NA, bg=grey(0.1,0.5))

    points(xpos, 100*mean(unlist(a_S_ME[[cc]]),na.rm=TRUE), pch=21, col="red"  , bg=rgb(1, 0, 0, 0.5), cex=1.5)

    mtext(cc, 1, -1.7, at=xpos, cex=0.7)

    xpos <- xpos + 1
}

mtext("Sormunen categories vs ME", 1,   -1, at=3, cex=0.7)

xpos <- xpos + 0.5  # skip one

points(rep(xpos, length(a_T_ME)), 100*unlist(a_T_ME), pch=21,col=NA, bg=grey(0.1,0.5))
points(xpos, 100*mean(unlist(a_T_ME),na.rm=TRUE), pch=21, col="red"  , bg=rgb(1, 0, 0, 0.5), cex=1.5)
mtext("TREC 0 or 1", 1,-1.7, at=xpos, cex=0.7)
mtext("vs ME"    , 1,  -1, at=xpos, cex=0.7)

xpos <- xpos + 2  # skip one

points(rep(xpos, length(a_T_S)), 100*unlist(a_T_S), pch=21,col=NA, bg=grey(0.1,0.5))
points(xpos, 100*mean(unlist(a_T_S),na.rm=TRUE), pch=21, col="red"  , bg=rgb(1, 0, 0, 0.5), cex=1.5)
mtext("TREC 0 or 1", 1, -1.7, at=xpos, cex=0.7)
mtext("vs N or {M,R,H}", 1, -1, at=xpos, cex=0.7)

dev.off()
options(errors=NULL)


