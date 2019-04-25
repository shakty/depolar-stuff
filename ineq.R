library(foreign)
library(ggplot2)
library(gridExtra)
library(Rmisc)
library(psych)
library(scales)
library(reshape2)
library(grid)
library(plyr)
library(data.table)
library(ineq)
library(texreg)
library(zoo)

DATA.DIR  <- '../DATA/'

IMGDIR <- './imgs/'

####################################################################
## GSS

read.dct <- function(dct, labels.included = "yes") {
    temp <- readLines(dct)
    temp <- temp[grepl("_column", temp)]
    switch(labels.included,
           yes = {
               pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+)[a-z]\\s+(.*)"
               classes <- c("numeric", "character", "character", "numeric", "character")
               N <- 5
               NAMES <- c("StartPos", "Str", "ColName", "ColWidth", "ColLabel")
           },
           no = {
               pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+).*"
               classes <- c("numeric", "character", "character", "numeric")
               N <- 4
               NAMES <- c("StartPos", "Str", "ColName", "ColWidth")
           })
    temp_metadata <- setNames(lapply(1:N, function(x) {
        out <- gsub(pattern, paste("\\", x, sep = ""), temp)
        out <- gsub("^\\s+|\\s+$", "", out)
        out <- gsub('\"', "", out, fixed = TRUE)
        class(out) <- classes[x] ; out }), NAMES)
    temp_metadata[["ColName"]] <- make.names(gsub("\\s", "", temp_metadata[["ColName"]]))
    temp_metadata
}

read.dat <- function(dat, metadata_var, labels.included = "yes") {
    read.fwf(dat, widths = metadata_var[["ColWidth"]], col.names = metadata_var[["ColName"]])
}


GSS.DIR <- paste0(DATA.DIR, 'GSS/')


GSS_metadata <- read.dct(paste0(GSS.DIR, "GSS.dct"))
GSS_ascii <- read.dat(paste0(GSS.DIR, "GSS.dat"), GSS_metadata)
attr(GSS_ascii, "col.label") <- GSS_metadata[["ColLabel"]]
GSS <- GSS_ascii

GSS$PARTYID.cut3 <- ifelse(GSS$PARTYID %in% c(0,1), "Democrat",
                    ifelse(GSS$PARTYID %in% c(2,3,4), "Independent",
                    ifelse(GSS$PARTYID %in% c(5,6), "Republican", "Other/NA")))

GSS$PARTYID.cut3 <- factor(GSS$PARTYID.cut3, levels=c("Republican",
                                                      "Independent",
                                                      "Democrat",
                                                      "Other/NA"))



#####################################################################
## World Bank Open Data


WBOD.DIR <- paste0(DATA.DIR, 'WorldBankOpenData/')

WBOD <- read.csv(paste0(WBOD.DIR, "API_SI.POV.GINI_DS2_en_csv_v2_10224868.csv"),
                 head=TRUE)

## Metadata_Country_API_SI.POV.GINI_DS2_en_csv_v2_10224868.csv
## Metadata_Indicator_API_SI.POV.GINI_DS2_en_csv_v2_10224868.csv

WBOD <- WBOD[WBOD$Country.Code == "USA",]

WBOD.melt <- melt(WBOD, id=c("Country.Name", "Country.Code",
                             "Indicator.Name", "Indicator.Code"))

WBOD.melt$variable <- as.character(WBOD.melt$variable)
WBOD.melt$variable <- gsub('X', '', WBOD.melt$variable)
WBOD.melt$variable <- as.numeric(WBOD.melt$variable)

WBOD.melt <- WBOD.melt[WBOD.melt$variable > 1978 &
                       WBOD.melt$variable < 2017 &
                       !is.na(WBOD.melt$variable),]
WBOD.melt$approx <- as.factor(ifelse(is.na(WBOD.melt$value), 1, 0))
WBOD.melt$gini.all <- na.approx(WBOD.melt$value)

WBOD.melt$Country.Name <- NULL
WBOD.melt$Country.Code <- NULL
WBOD.melt$Indicator.Name <- NULL
WBOD.melt$Indicator.Code <- NULL
WBOD.melt$variable2 <- NULL
WBOD.melt$gini <- WBOD.melt$value
WBOD.melt$value <- NULL
WBOD.melt$YEAR <- WBOD.melt$variable
WBOD.melt$variable <- NULL

p0 <- ggplot(WBOD.melt, aes(YEAR, gini.all)) +
    geom_line(aes(group=1)) + geom_point(size=4, aes(color=approx)) + 
    xlab("Year") + ylab("Gini") + 
    theme_bw() +
    ggtitle("Gini Coefficient (blue dots interpolations)") +
    theme(
        legend.position="none"
        ##axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
    )

## Merging GSS and WBOD

jpeg(paste0(IMGDIR, "ineq_trend_by_party_GINI.jpg"), width=800, height=800)
grid.arrange(p0, p1)
dev.off()



GSS <- merge(GSS, WBOD.melt, by="YEAR", all.x=TRUE)

###################################
## GSS Analysis

ggplot(GSS, aes(YEAR, EQWLTH)) +
    geom_jitter(alpha=0.5) +
    geom_smooth(method="lm", color="red") +
    geom_smooth() +
    theme_bw()


mydata <- GSS[GSS$EQWLTH > 0 & GSS$EQWLTH < 8,]
mydata$EQWLTH <- abs(mydata$EQWLTH - max(mydata$EQWLTH))+1
mysm <- summarySE(mydata, "EQWLTH", "YEAR", na.rm=TRUE)
p8 <- ggplot(mysm[mysm$sd != 0,], aes(YEAR, EQWLTH)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin=EQWLTH-ci, ymax=EQWLTH+ci)) +
    theme_bw() +
    ylim(3,5) +
    ggtitle("8. Should gov. reduce income diff.? (1-7)")

mydata <- GSS[GSS$GOODLIFE < 6 & GSS$GOODLIFE > 0,]
mydata$GOODLIFE <- abs(mydata$GOODLIFE - max(mydata$GOODLIFE))+1
mysm <- summarySE(mydata, "GOODLIFE", "YEAR", na.rm=TRUE)
p2 <- ggplot(mysm[mysm$sd != 0,], aes(YEAR, GOODLIFE)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin=GOODLIFE-ci, ymax=GOODLIFE+ci)) +
    theme_bw() +
    ylim(2,5) +
    ggtitle("2. Standards of living will improve? (1-5)")

mydata <- GSS[GSS$HELPPOOR < 6 & GSS$HELPPOOR > 0,]
mydata$HELPPOOR <- abs(mydata$HELPPOOR - max(mydata$HELPPOOR))+1
mysm <- summarySE(mydata, "HELPPOOR", "YEAR", na.rm=TRUE)
p3 <- ggplot(mysm[mysm$sd != 0,], aes(YEAR, HELPPOOR)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin=HELPPOOR-ci, ymax=HELPPOOR+ci)) +
    theme_bw() +  
    ylim(2, 5) +
    ggtitle("3. Should gov. improve standard of living? (1-5)")


mydata <- GSS[GSS$RACDIF4 < 3 & GSS$RACDIF4 > 0 ,]
mydata$RACDIF4 <- abs(mydata$RACDIF4 - max(mydata$RACDIF4))
mysm <- summarySE(mydata, "RACDIF4", "YEAR", na.rm=TRUE)
p4 <- ggplot(mysm[mysm$sd != 0,], aes(YEAR, RACDIF4)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin=RACDIF4-ci, ymax=RACDIF4+ci)) +
    theme_bw() +
    ylim(0, 1) +
    ggtitle("4. Black lacks will? (0/1)")


mydata <- GSS[GSS$RACDIF3 < 3 & GSS$RACDIF3 > 0,]
mydata$RACDIF3 <- abs(mydata$RACDIF3 - max(mydata$RACDIF3))
mysm <- summarySE(mydata, "RACDIF3", "YEAR", na.rm=TRUE)
p5 <- ggplot(mysm[mysm$sd != 0,], aes(YEAR, RACDIF3)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin=RACDIF3-ci, ymax=RACDIF3+ci)) +
    theme_bw() +
    ylim(0, 1) +
    ggtitle("5. Black lacks education? (0/1)")

mydata <- GSS[GSS$RACDIF2 < 3 & GSS$RACDIF2 > 0,]
mydata$RACDIF2 <- abs(mydata$RACDIF2 - max(mydata$RACDIF2))
mysm <- summarySE(mydata, "RACDIF2", "YEAR", na.rm=TRUE)
p6 <- ggplot(mysm[mysm$sd != 0,], aes(YEAR, RACDIF2)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin=RACDIF2-ci, ymax=RACDIF2+ci)) +
    theme_bw() +
    ylim(0, 1) +
    ggtitle("6. Black are inherently worse? (0/1)")


mydata <- GSS[GSS$RACDIF1 < 3 & GSS$RACDIF1 > 0,]
mydata$RACDIF1 <- abs(mydata$RACDIF1 - max(mydata$RACDIF1))
mysm <- summarySE(mydata, "RACDIF1", "YEAR", na.rm=TRUE)
p7 <- ggplot(mysm[mysm$sd != 0,], aes(YEAR, RACDIF1)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin=RACDIF1-ci, ymax=RACDIF1+ci)) +
    theme_bw() +
    ylim(0, 1) +
    ggtitle("7. Black are discriminated? (0/1)")

grid.arrange(p8, p2, p3, p4, p5, p6, p7, nrow = 3)

jpeg("./ineq_trend.jpg", width=800, height=800)
grid.arrange(p8, p2, p3, p4, p5, p6, p7, nrow = 3)
dev.off()



## Complicated by Income because it changes meaning over time.
## Check guide, and methodological report 56.

## PARTY ID

## Messy with all data.
mydata <- GSS[GSS$EQWLTH > 0 & GSS$EQWLTH < 8 &
              GSS$PARTYID.cut3 != "Other/NA",]
mydata$EQWLTH <- abs(mydata$EQWLTH - max(mydata$EQWLTH))+1
mysm <- summarySE(mydata, "EQWLTH", c("YEAR", "PARTYID.cut3"), na.rm=TRUE)
p1 <- ggplot(mysm[mysm$sd != 0,], aes(YEAR, EQWLTH,
                                group=PARTYID.cut3, color=PARTYID.cut3)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin=EQWLTH-ci, ymax=EQWLTH+ci)) +
    theme_bw() +
    ##ylim(3,5) +
    ##facet_wrap(~PARTYID) +
    ggtitle("8. Should gov. reduce income diff.? (1-7)") +
    theme(legend.position="none")


mydata <- GSS[GSS$GOODLIFE < 6 & GSS$GOODLIFE > 0 &
              GSS$PARTYID.cut3 != "Other/NA",]
mydata$GOODLIFE <- abs(mydata$GOODLIFE - max(mydata$GOODLIFE))+1
mysm <- summarySE(mydata, "GOODLIFE",  c("YEAR", "PARTYID.cut3"), na.rm=TRUE)
p2 <- ggplot(mysm[mysm$sd != 0,], aes(YEAR, GOODLIFE,
                                      group=PARTYID.cut3, color=PARTYID.cut3))+
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin=GOODLIFE-ci, ymax=GOODLIFE+ci)) +
    theme_bw() +
    ylim(2,5) +
    ggtitle("2. Standards of living will improve? (1-5)") +
    theme(legend.position="none")

mydata <- GSS[GSS$HELPPOOR < 6 & GSS$HELPPOOR > 0 &
              GSS$PARTYID.cut3 != "Other/NA",]
mydata$HELPPOOR <- abs(mydata$HELPPOOR - max(mydata$HELPPOOR))+1
mysm <- summarySE(mydata, "HELPPOOR", c("YEAR", "PARTYID.cut3"), na.rm=TRUE)
p3 <- ggplot(mysm[mysm$sd != 0,], aes(YEAR, HELPPOOR,
                                      group=PARTYID.cut3, color=PARTYID.cut3)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin=HELPPOOR-ci, ymax=HELPPOOR+ci)) +
    theme_bw() +  
    ylim(2, 5) +
    ggtitle("3. Should gov. improve standard of living? (1-5)") +
    theme(legend.position="bottom")

grid.arrange(p1, p2, p3, nrow = 2)

jpeg("./ineq_trend_by_party.jpg", width=800, height=800)
grid.arrange(p1, p2, p3, nrow = 2)
dev.off()

