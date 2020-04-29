## using devel version of GLMMmisc package
## devtools::install_github("pcdjohnson/GLMMmisc")
## library(GLMMmisc)
## library(lmerTest)
## library(parallel)
## library(reshape2)
## library(pheatmap)
## library(ggplot2)


##' Simulate data with random variance for wolves and locations
##'
##' Simulate data...
##' @title simulate.wolfData
##' @param sample.size sample size
##' @param effMF sex effect size 
##' @param effDens wolf density effect size
##' @return 
##' @author Emanuel Heitlinger
simulate.wolfData <- function(sample.size, effMF, effDens){
    wolfData <- expand.grid(prey.sex=c("F", "M"),
                            location=letters[1:10],
                            wolf=1:20)

    wolfData$wolf <- paste0(wolfData$location, wolfData$wolf)

    wolfData$location.wolf.density=as.factor(rep(c("low", "high"),
                                                 each=2))

    wolfData$row.id <- 1:nrow(wolfData)


    sampling.n <- sample.size/nrow(wolfData)

    wolfData<-
        sim.glmm(
            design.data = wolfData,
            fixed.eff =
                list(intercept = log(sampling.n), # sampling.n samples per wolf
                     location.wolf.density = log(c(
                         low = effDens, ## a low wolf density effect
                         high = 1)),
                     prey.sex = log(c(
                         F = effMF,
                         M = 1))
                     ), 
            rand.V =
                inv.mor(
                    c(## row.id = 2,   # the overdispersion median
                                    # rate ratio (MRR) is 2
                      location = 1.3,# there is 1.3 -fold variation in
                      wolf = 1.3)),   # abundance between packs and 1.3 fold between wolves
            distribution = "poisson") # we are simulating a
                                        # Poisson response (count
                                        # females and males per
                                        # wolf
    wolfData
}

