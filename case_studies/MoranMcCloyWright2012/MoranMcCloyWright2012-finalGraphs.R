#! /usr/bin/env Rscript

## This script generates the statistical model and figures for:
##
## Moran, S., McCloy, D. R., & Wright, R. A. (2012). Revisiting population size
## vs. phoneme inventory size. Language, 88(4), 877–893.
## http://doi.org/10.1353/lan.2012.0087
##
## This is a cleaned-up and better-annotated version of the original script used
## at the time of writing that paper, but the results are essentially the same.

## set global options (to be restored at end)
saf <- getOption("stringsAsFactors")
options(stringsAsFactors=FALSE)

## libraries
library(lme4)
library(Hmisc)  # provides rcorr

## error bar function
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
    if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
        stop("vectors must be same length")
    arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

## plotting parameters, so we can adjust all plots at once
dotcol <- rgb(0, 0, 0, 0.2)
dotcex <- 0.5
legendcex <- 1
legendinset <- 0.2
lowesswidth <- 1.25
axiscol <- rgb(0.3, 0.3, 0.3, 1)
axiscex <- 1
axislwd <- 0.25
axistck <- -0.02
axislabelcex <- 1
axislabelpos <- 2.25
axismgp <- c(2, 0.25, 0)
axis2mgp <- c(2, 0.4, 0)
font <- "FreeSerif"

## load main data
rawdata <- read.delim("MoranMcCloyWright2012-data.tsv", header=TRUE, sep="\t",
                      quote="\"", dec=".", skip=4)
rawdata$logPho <- log10(rawdata$pho)
rawdata$logCon <- log10(rawdata$con)
rawdata$logVow <- log10(rawdata$vow)
rawdata$son[is.na(rawdata$son)] <- 0
## load ISO - multitree mapping
setwd("family-tables")
eth <- read.delim("eth15iso-mtfam.tsv", header=T, sep="\t", quote="\"", dec=".")
## load list of families used by Hay & Bauer 2007
hb <- read.delim("famsHayAndBauer.tsv", header=T, sep="\t", quote="\"", dec=".")
## load exclusion lists
excludeIsol <- read.delim("famsMassedIsolates.tsv", header=TRUE)
excludeMixd <- read.delim("famsMixedLanguages.tsv", header=TRUE)
excludePidg <- read.delim("famsPidginCreole.tsv", header=TRUE)
excludeUncl <- read.delim("famsUnclassified.tsv", header=TRUE)
excludeSign <- read.delim("famsSignedLanguages.tsv", header=TRUE)
excludeExtc <- read.delim("famsExtinctAncient.tsv", header=TRUE)
setwd("..")
## merge exclusion lists
excludeAll <- do.call(rbind, list(excludeMixd, excludePidg, excludeIsol,
                                  excludeUncl, excludeSign, excludeExtc,
                                  c("UNCLASSIFIED", "Unclassified")))
## apply exclusion list
eth.pruned <- eth[!eth$fam %in% excludeAll$fam,]       # 7097 -> 6956
hb.pruned <- hb[!hb$fam %in% excludeAll$fam,]          #  216 ->  211
rawdata <- rawdata[!rawdata$fam %in% excludeAll$fam,]  # 969 -> 968

## ## ## ## ## ## ## ## ## ## ## ##
##  ETHNOLOGUE VS PHOIBLE VS HB  ##
## ## ## ## ## ## ## ## ## ## ## ##
## frequency tables
eth.counts <- as.data.frame(table(eth.pruned$fam))
hb.counts <- as.data.frame(table(hb.pruned$fam))
phoible <- as.data.frame(table(rawdata$fam))
## comparison table: Ethnologue vs. PHOIBLE vs. Hay & Bauer
comparator <- merge(eth.counts, phoible, by="Var1", all=TRUE)
comparator <- merge(comparator, hb.counts, by="Var1", all=TRUE)
colnames(comparator) <- c("fam", "ethnologue", "phoible", "haybauer")
comparator[is.na(comparator)] <- 0
comparator <- comparator[with(comparator, order(ethnologue, phoible, haybauer)),]
## log-transformed within-family language counts
comparator$ethno.log <- with(comparator, ifelse(ethnologue==0, 0, log10(ethnologue)))
comparator$phoible.log <- with(comparator, ifelse(phoible==0, 0, log10(phoible)))
comparator$haybauer.log <- with(comparator, ifelse(haybauer==0, 0, log10(haybauer)))
## label staggering for graph
evenLabels <- comparator$fam[c(FALSE, TRUE)]
oddLabels <- comparator$fam[c(TRUE, FALSE)]
## FIGURE 4: LOG-TRANSFORMED PLOT OF ETHNOLOGUE VS PHOIBLE VS H&B
svg("figure4.svg", width=6.5, height=4.5, pointsize=12, family=font)
par(mar=c(4,3,0,0), oma=c(0,0,0,0), lwd=0.5)
## graph colors
gray85 <- hcl(h=0, c=0, l=85, alpha=1)
gray70 <- hcl(h=0, c=0, l=70, alpha=1)
gray55 <- hcl(h=0, c=0, l=55, alpha=1)
gray50 <- hcl(h=0, c=0, l=50, alpha=1)
gray20 <- hcl(h=0, c=0, l=20, alpha=1)
## plot
plot(1:nrow(comparator), comparator$haybauer.log, type="n", lty=3,
     frame.plot=FALSE, ann=FALSE, axes=FALSE, ylim=c(0,3.5))
polygon(c(1:nrow(comparator), nrow(comparator)), c(comparator$ethno.log, 0),
        col=gray85, lty=1)
polygon(c(1:nrow(comparator), nrow(comparator)), c(comparator$phoible.log, 0),
        col=gray70, lty=1)
polygon(c(1:nrow(comparator), nrow(comparator)), c(comparator$haybauer.log, 0),
        col=gray55, lty=1)
axis(1, at=seq(1, nrow(comparator), 2), labels=oddLabels, las=3, lwd=0, lwd.tick=0.25,
     col=gray50, col.ticks=gray50, cex.axis=0.45, tck=0.005, mgp=c(3, -0.1, 1))
axis(1, at=seq(2, nrow(comparator), 2), labels=evenLabels, las=3, lwd=0, lwd.tick=0.25,
     col=gray50, col.ticks=gray50, cex.axis=0.45, tck=-0.005, mgp=c(3, 1.2, 1))
axis(1, at=1:nrow(comparator), lwd=0.25, lwd.tick=0, labels=FALSE, col.axis=gray50,
     mgp=c(3, 0, 1))
axis(2, at=seq(0, 3.5, 0.5), las=1, col.axis=gray20, col=gray50, col.ticks=gray50,
     cex.axis=0.75, tck=-.0075, lwd=0.25, mgp=c(2, 0.4, 0))
mtext("Language family codes", side=1, line=2.4, cex=1)
mtext("log₁₀(number of languages)", side=2, line=2, cex=1)
legend("topleft", legend=c("Ethnologue", "Phoible", "Hay & Bauer"),
       fill=c(gray85, gray70, gray55), bty="n", inset=0.05, cex=1)
dev.off()


## ## ## ## ## ## ## ## ## ## ##
## WITHIN-FAMILY CORRELATIONS ##
## ## ## ## ## ## ## ## ## ## ##
## The model specification in this file has changed slightly from the model used
## at the time the paper was written, because the previous model specification
## (originally run with lme4::lmer version 0.999999-0) does not converge with
## current versions of lme4::lmer (version 1.1.7 as of 2015.04.29). Consequently
## the estimates for the intercept and slope of each language family group have
## slightly changed, but the substance of the results and the conclusions drawn
## remain intact. This was the original model:
## loglogPho <- lmer(logPho ~ logPop + (1+logPop|genus) + (1+logPop|fam), data=rawdata)
## This is the model that works with modern lme4 (versions > 1.0)
loglogPho <- lmer(logPho ~ logPop + (1+logPop|fam/genus), data=rawdata)
## extract coefficients for each family, for later plotting
groupCoefficients <- data.frame(coef(loglogPho)$fam)
grandIntercept <- unlist(fixef(loglogPho)[1])
grandSlope <- unlist(fixef(loglogPho)[2])
## collapsing each family to its average
langPerFam <- data.frame(table(rawdata[!is.na(rawdata$pop), "fam"]))
colnames(langPerFam) <- c("fam", "langs")
langPerFam$sumPho <- sapply(langPerFam$fam, function(i) sum(rawdata[rawdata$fam == i, "pho"]))
langPerFam$sumPop <- sapply(langPerFam$fam, function(i) sum(rawdata[rawdata$fam == i, "pop"]))
langPerFam$avgPho <- langPerFam$sumPho / langPerFam$langs
langPerFam$avgPop <- langPerFam$sumPop / langPerFam$langs
langPerFam$logAvgPop <- log10(langPerFam$avgPop)
## the 12 largest families (by representation in PHOIBLE data)
top12 <- merge(rawdata[rawdata$fam %in% langPerFam[order(langPerFam$langs,
                                                         decreasing=TRUE),
                                                   "fam"][1:12],],
               with(langPerFam, data.frame(fam=fam, famCount=langs)),
               by="fam", all.x=TRUE)
## mixed model coefficients for the top 12
top12.coefs <- groupCoefficients[unique(top12$fam),]
famNames <- c(afas="Afro-Asiatic", altc="Altaic", anes="Austronesian",
              ausa="Austro-Asiatic", aust="Australian", ieur="Indo-European",
              ncon="Niger-Congo", nsah="Nilo-Saharan", otma="Otomanguean",
              sitb="Sino-Tibetan", trng="Trans-New Guinea",
              utaz="Uto-Aztecan")
top12.coefs$famName <- famNames[rownames(top12.coefs)]
## FIGURE 2: 3x4 LATTICE PLOT OF WITHIN-FAMILY CORRELATIONS
svg("figure2.svg", width=6.5, height=9, pointsize=12, family=font)
par(mfrow=c(4, 3), mar=c(3.5, 4, 3, 0), omi=c(0, 0, 0, 0.25))
for (i in 1:nrow(top12.coefs)) {
    subdata <- top12[top12$fam %in% rownames(top12.coefs)[i],]
    plot(subdata$logPop, subdata$logPho, frame.plot=FALSE, axes=FALSE, ann=FALSE,
         col=dotcol, pch=16, cex=dotcex, xlim=c(0, 10), ylim=c(1, 2))
    abline(a=top12.coefs[i, 1], b=top12.coefs[i, 2], col='black', lwd=1.5)
    axis(1, at=seq(0, 10, 2), las=1, col.axis=axiscol, col.tick=axiscol,
         col=axiscol, cex.axis=axiscex, lwd=axislwd, tck=axistck, mgp=axismgp)
    axis(2, at=seq(1, 2, 0.2), las=1, col.axis=axiscol, col.tick=axiscol,
         col=axiscol, cex.axis=axiscex, lwd=axislwd, tck=axistck, mgp=axis2mgp)
    if (i > 9) {
        mtext("log\u2081\u2080(population)", side=1, line=2, cex=1)
    }
    if (i %% 3 == 1) {
        mtext("log\u2081\u2080(phonemes)", side=2, line=2.5, cex=1)
    }
    mtext(famNames[rownames(top12.coefs)[i]], side=3, line=0.75, cex=1, font=2)
    mtext(paste("slope = ", round(top12.coefs[i,2], digits=5), sep=""),
          side=3, line=-0.5, cex=0.75, font=1)
}
dev.off()


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##  MEAN AND VARIANCE BY ORDER OF MAGNITUDE  ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
rawdata$decade <- ceiling(rawdata$logPop)
decadeTable <- data.frame(table(rawdata$decade))
colnames(decadeTable) <- c('decade','numlangs')
decadeTable$decade <- as.numeric(as.character(decadeTable$decade))
decadeTable$phonavg <- sapply(decadeTable$decade, function(i) mean(rawdata[rawdata$decade == i,]$pho))
decadeTable$phonvar <- sapply(decadeTable$decade, function(i) var(rawdata[rawdata$decade == i,]$pho))
decadeTable$phonsd <- sqrt(decadeTable$phonvar)
decadeTable$lower <- decadeTable$phonavg-decadeTable$phonsd
decadeTable$upper <- decadeTable$phonavg+decadeTable$phonsd
decadeTable$labels <- c("1","<10","<10²","<10³","<10⁴","<10⁵","<10⁶","<10⁷","<10⁸","<10⁹")
decadeTable$labels2 <- c("1","10","10²","10³","10⁴","10⁵","10⁶","10⁷","10⁸","10⁹")
## FIGURE 3: MEANS AND SDs BY POPULATION-SIZE COHORT
svg("figure3.svg", width=6.5, height=6.5, pointsize=12, family=font)
par(omi=c(0, 0, 0, 0), mar=c(2, 3, 0, 0), lwd=0.5, font=1)
plot(0, 0, type="n",frame.plot=FALSE, axes=FALSE, ann=FALSE,
     xlim=c(-0.8, 9.2), ylim=c(0, 90))
## scale bar
arrows(-0.6, 10^grandIntercept, -0.6, 10^grandIntercept+9*10^grandSlope,
       col='black', lwd=2, lend="butt", length=0)
arrows(-0.6, 10^grandIntercept+9*10^grandSlope+1, 0.5, 68,
       col=axiscol, lwd=1, length=0.1, code=1)
text(0.5, 70, labels="magnitude of predicted effect\n = 9.19 phonemes",
     pos=4, offset=0.5, col='black', cex=1)
## means & SDs
error.bar(decadeTable$decade, decadeTable$phonavg, decadeTable$phonsd,
          col='black', lwd=0.75, length=0.05)
points(decadeTable$decade, decadeTable$phonavg, pch=21, col='black', bg='white')
axis(1, at=c(0:9), las=1, cex.axis=axiscex, labels=decadeTable$labels,
     col.ticks=axiscol, col.axis=axiscol, col=axiscol, lwd=axislwd, tck=axistck/3,
     mgp=axismgp, pos=0)
axis(2, at=seq(0,90,10), las=1, cex.axis=axiscex, col.ticks=axiscol, col.axis=axiscol,
     col=axiscol, lwd=axislwd, tck=axistck/3, mgp=axis2mgp)
mtext("Speaker Population (Grouped by Decade)", side=1, line=0.6, col='black', cex=1)
mtext("Mean Phoneme Inventory Size (± 1 s.d.)", side=2, line=2, col='black', cex=1)
dev.off()


## ## ## ## ## ## ## ## ## ## ## ## ## ##
## REPRODUCING HAY AND BAUER'S RESULTS ##
## ## ## ## ## ## ## ## ## ## ## ## ## ##
## Note that in the HMISC package p=0 means p<0.0001;
## hence the ifelse() statement in the legend section below.
# FIGURE 1: LATTICE PLOT OF LOWESS H&B REPRODUCTION
svg("figure1.svg", width=6.5, height=9, pointsize=12, family=font)
par(mar=c(4,4,1,1), omi=c(0,0,0,0), lwd=0.5)
## relative amount of space that goes to each of the seven plots
layout(matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,
                3,3,3,3,3,3,3,8,8,4,4,4,4,4,4,4,4,4,4,4,4,0,5,5,5,5,5,5,5,
                0,6,6,6,6,6,6,6,0,7,7,7,7,7,7,7,8,8), 38, 2, byrow=FALSE))
for (i in which(colnames(rawdata) %in% c("pho", "con", "son", "obs", "vow", "mon", "qua"))) {
    ymax <- switch(colnames(rawdata)[i], pho=150, con=, obs=100, son=, vow=, mon=, qua=50)
    name <- switch(colnames(rawdata)[i], pho="Phonemes", con="Consonants",
                   son="Sonorant Cons.", obs="Obstruents", vow="All Vowels",
                   mon="Monophthongs", qua="Vowel Qualities")
    axislabel <- switch(colnames(rawdata)[i], son=, qua="log\u2081\u2080(Population)", "")
    plot(rawdata$logPop, rawdata[,i], ann=FALSE, axes=FALSE, frame.plot=FALSE,
         col=dotcol, pch=16, cex=dotcex, xlim=c(1, 10), ylim=c(0, ymax), asp=0.1)
    lines(lowess(rawdata$logPop, rawdata[,i]), lwd=lowesswidth)
    spear <- rcorr(as.matrix(rawdata[c(which(colnames(rawdata) %in% "logPop"), i)]),
                   type="spearman")
    rho <- round(spear$r[1, 2], digits=4)
    pval <- round(spear$P[1, 2], digits=6)
    pv <- log10(pval)
    pvineq <- ifelse(pv > -1.30103, paste("p > 0.05"),
                     ifelse(pv > -2, paste("p < 0.05"),
                            ifelse(pv > -4, paste("p <",10^trunc(pv)),
                                   paste("p < 0.0001"))))
    leg <- paste("Spearman's \u03C1 =", rho, "\n", pvineq)
    legend("topleft", legend=leg, bty="n", inset=legendinset, cex=legendcex)
    axis(1, at=c(0:10), las=1, col.axis=axiscol, col.ticks=axiscol, cex.axis=axiscex,
         lwd=axislwd, tck=axistck/3, mgp=axismgp)
    axis(2, at=seq(0, ymax, 10), las=1, col.axis=axiscol, col.ticks=axiscol,
         cex.axis=axiscex, lwd=axislwd, tck=axistck/6, mgp=axis2mgp)
    mtext(axislabel, side=1, line=axislabelpos-0.5, cex=axislabelcex)
    mtext(name, side=2, line=axislabelpos, cex=axislabelcex)
}
dev.off()


## reset options
options(stringsAsFactors=saf)
