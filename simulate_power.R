## devtools::install_github("pcdjohnson/GLMMmisc")
library(GLMMmisc)
library(lmerTest)
library(parallel)
library(reshape2)
library(pheatmap)
library(gridExtra)
library(tidyr)

set.seed(123) 

simulate.wolfData <- function(sample.size, effMF, effDens){
    wolfData <- expand.grid(prey.sex=c("F", "M"),
                            location=letters[1:14],
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
                    c(location = 1.3, # there is 30%variation in
                      wolf = 1.3)),   # abundance between packs and 30% between wolves
            distribution = "poisson") # we are simulating a
                                        # Poisson response (count
                                        # females and males per
                                        # wolf
    wolfData
}

## sample size
ssizes <- c(500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000)
names(ssizes) <- paste0("size_", ssizes)

n.reps <- 100

## Sex ####################################

## plain sex effect size: female same (1) or up to 1.5-fold more in prey 
sex <- seq(1, 1.5, by=0.05)
names(sex) <- paste0("sex_", sex)

sex.p <- mclapply(ssizes, function(s.size){
    mclapply(sex, function(sexEff){
        ## effect size: female same (1) up to 2-fold more high density
        reps <- sapply(1:n.reps, function (i){
            dat <- simulate.wolfData(s.size, effMF=sexEff, effDens=1)
            mod <- lme4::glmer(response ~ prey.sex + location.wolf.density +
                                   (1 | location) + (1 | wolf), 
                               family = 'poisson', data = dat)
            sW <- summary(mod)
            sW$coefficients[2, "Pr(>|z|)"] > 0.05
        })
        sum(reps)/n.reps
    }, mc.cores=3)
}, mc.cores=7)



powerS <- melt(sex.p)

names(powerS) <- c("power", "effSizeSex", "sampleSize")

powerS$effSizeSex <- as.numeric(gsub("sex_", "", powerS$effSizeSex))

powerS$effSizeSex <- factor(paste0((powerS$effSizeSex), "*"),
                            levels=unique(paste0((powerS$effSizeSex), "*")))

powerS$sampleSize <- as.factor(as.numeric(gsub("size_" ,"", powerS$sampleSize)))

powerS  <- spread(powerS, effSizeSex, power)

rownames(powerS) <- powerS$sampleSize

powerS$sampleSize <- NULL

sexPheat <- pheatmap(powerS, cluster_rows=FALSE, cluster_cols=FALSE,
                     main = paste("Proportion of false negative findings for\n",
                                  "deviations in prey sex from 50%"))

png("figures/Sex_power.png", width=5, height=5, units = 'in', res = 300)
grid.arrange(grobs = list(sexPheat[[4]]),
             ## list(sexPheat[[4]][-3,]) for removing legend?
             right = textGrob("Sample size",  rot=270),
             bottom = textGrob(paste0("Rate ratio for female over male prey\n", 
                                      "(fold increase of female over male)")))
dev.off()



## Location ################################ location effect size:
## female same (1) up to 2-fold more in prey for low density regions
loc <- seq(1, 2, by=0.1)
names(loc) <- paste0("loc_", loc)

loc.p <- mclapply(ssizes, function(s.size){
    mclapply(loc, function(locEff){
        reps <- sapply(1:n.reps, function (i){
            dat <- simulate.wolfData(s.size, 1, locEff)
            mod <- lme4::glmer(response ~ prey.sex + location.wolf.density +
                                   (1 | location) + (1 | wolf), 
                               family = 'poisson', data = dat)
            sW <- summary(mod)
            sW$coefficients[3, "Pr(>|z|)"]>0.05
        })
        sum(reps)/n.reps
    }, mc.cores=3)
}, mc.cores=7)



powerL <- melt(loc.p)

names(powerL) <- c("power", "effSizeLoc", "sampleSize")

powerL$effSizeLoc <- as.numeric(gsub("loc_", "", powerL$effSizeLoc))

powerL$effSizeLoc <- factor(paste0((powerL$effSizeLoc), "*"),
                            levels=unique(paste0((powerL$effSizeLoc), "*")))

powerL$sampleSize <- as.factor(as.numeric(gsub("size_" ,"", powerL$sampleSize)))

powerL  <- spread(powerL, effSizeLoc, power)

rownames(powerL) <- powerL$sampleSize

powerL$sampleSize <- NULL


locPheat <- pheatmap(powerL, cluster_rows=FALSE, cluster_cols=FALSE,
                     main = paste("Proportion of false negative findings for\n",
                                  "deviations in prey sex at 7 of 14 locations"))

png("figures/Loc_power.png", width=5, height=5, units = 'in', res = 300)

grid.arrange(grobs = list(locPheat[[4]]),
             ## list(sexPheat[[4]][-3,]) for removing legend?
             right = textGrob("Sample size",  rot=270),
             bottom = textGrob(paste0("Rate ratio for female over male prey\n", 
                                      "(fold increase of female over male)")))

dev.off()
