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
library(maps)

## FONT for plots
theme_set(theme_bw(base_size = 20))

myThemeMod <- theme(axis.title.x = element_text(vjust=-1, size=24),
                    axis.title.y = element_text(vjust=-0.1, size=24),
                    plot.margin=unit(c(10,10,10,10),"mm"),
                    plot.title = element_text(vjust=3, size=24,face="bold"),
                    legend.background = element_rect(fill = "white", color="grey"),
                    legend.title = element_blank(),
                    #legend.title = element_text(vjust=3, size=16, face="bold"),
                    #legend.direction = "horizzontal",
                    legend.text = element_text(size=16),
                    legend.key.width = unit(1.5, "cm"),
                    legend.key = element_rect(fill = "white", colour = "white")
)




if(.Platform$OS.type == "unix") {
  THISDIR <- '~/Dropbox/msr-ineq/R/'
} else {
  THISDIR <- 'C:/Users/stbaliet/Dropbox/msr-ineq/R/'
}
setwd(THISDIR)


data <- data.frame()
DATA.DIR  <- 'DATA'
dirs <- list.dirs(path=DATA.DIR)
for (dir in dirs) {
    if (dir != 'DATA/' && dir != 'DATA') {
        file  <- paste0(THISDIR, dir, '/player_data.csv')
        ## print(file)
        if (file.exists(file)) {
            tmp <- read.csv(file, sep=",", header = TRUE,
                              stringsAsFactors = FALSE)
            if (nrow(tmp) == 0) {
                print(paste0('!!!! empty file ', dir))
                next
            }
            ##print('before***')
            ##print(ncol(tmp))
            ##print(ncol(data))
            if (nrow(data) != 0) {
                ## Find names of missing columns
                missing <- setdiff(names(tmp), names(data))
                missing2 <- setdiff(names(data), names(tmp))
                origNames <- colnames(data)
                for (j in missing)  {
                    data = cbind(data,c(0))
                }
                colnames(data) <- c(origNames, missing)
                ##
                origNames <- colnames(tmp)
                for (j in missing2)  {
                    tmp = cbind(tmp, c(0))
                }
                colnames(tmp) <- c(origNames, missing2)
                ## Not working.
                ## data[missing,] <- NA
                ## tmp[missing2,] <- NA
            }
            ##print('after***')
            ##print(ncol(tmp))
            ##print(ncol(data))
            if (nrow(data) == 0) {
                data <- tmp
            } else {
                data <- rbind(data, tmp)
            }
            ## Test: write all single file in same dir.
            ##file <- paste0(THISDIR, DATA.DIR, '/ALL/', dir, '.csv')
            ##print(file)
            ##write.csv(data, file, sep=",", row.names=FALSE)            
        } else {
            print(paste0("!!!! ", dir, " has no file"))
        }
    } 
}

## Join Info From JSON.

fromjson <- read.csv(paste0(THISDIR, 'from_json.csv'))
data <- merge(data, fromjson, by="player")


## MID VALUE

MID <- 4

## Fixes

data$initials <- ifelse(data$initials == "TRUE", "T", ifelse(data$initials == "FALSE", "F", data$initials))

data$language <- ifelse(data$language == "English ", "English", data$language)
data$democratic <- ifelse(data$demrep < MID, 'Yes', 'No')
data$independent <- ifelse(data$demrep == MID, 'Yes', 'No')
data$republican <- ifelse(data$demrep > MID, 'Yes', 'No')
data$liberal  <-  ifelse(data$libcons < MID, 'Yes', 'No')
data$conservative  <- ifelse(data$libcons > MID, 'Yes', 'No')

##

if (!("session" %in% names(data))) {
    data$session  <- "pilot1"
    data$date <- "04-22-2019"
}

## Govred recode.

## Pilot 1 had scale inverted.
data$govred <- ifelse(data$session == "pilot1", 8 - data$govred, data$govred)
data$govred2 <- ifelse(data$session == "pilot1", 8 - data$govred2, data$govred2)


## Flag weird responses.

## These are weird only for the policy/govred.
WEIRD.RESPONSES <- c(
    "dL8m2W3h", ## Confused writing and decrease in support even if text hint the opposite.
    "xtrXBR5L", ## Maybe confused from 7 to 1
    "QbPTFyRk"  ## Faked essay (repeated sentence).
)

data$weird <- ifelse(data$player %in% WEIRD.RESPONSES, 1, 0)

## Save DB for similiraties to be processed by JS.
##################################################

## write.table(data, paste0(THISDIR, 'db.csv'), sep=",", row.names=FALSE)


##################################################


##

data$currentstate.lower <- tolower(data$currentstate)
data$candidate.all <- ifelse(data$candidate == 'Other', data$othercandidate, data$candidate)

data$candidate.all <- factor(data$candidate.all, 
                             levels=names(sort(table(data$candidate.all), 
                                               decreasing=TRUE)))
##

fun <- function(v) {
    res <- -1
    if (v == "Significantly decreased" ||
        v == "Significantly decrease") res <- 1
    if (v == "Moderately decreased" ||
        v == "Moderately decrease") res <- 2
    if (v == "Slightly decreased" ||
        v == "Slightly decrease") res <- 3
    if (v == "Left as is" || v == "Stay the same" || 
        v == 'Keep at current level') res <- 4
    if (v == "Slightly increased" ||
        v == "Slightly increase") res <- 5
    if (v == "Moderately increased" ||
        v == "Moderately increase") res <- 6
    if (v == "Significantly increased" ||
        v == "Significantly increase") res <- 7
    if (res == -1) stop('WTF')
    return(res)
}

data$estatetax.num <- unlist(lapply(data$estatetax, fun))
data$estatetax2.num <- unlist(lapply(data$estatetax2, fun))

data$billionaires.num <- unlist(lapply(data$billionaires, fun))
data$billionaires2.num <- unlist(lapply(data$billionaires2, fun))

data$aidpoor.num <- unlist(lapply(data$aidpoor, fun))
data$aidpoor2.num <- unlist(lapply(data$aidpoor2, fun))

data$minimalwage.num <- unlist(lapply(data$minimalwage, fun))
data$minimalwage2.num <- unlist(lapply(data$minimalwage2, fun))

data$publichousing.num <- unlist(lapply(data$publichousing, fun))
data$publichousing2.num <- unlist(lapply(data$publichousing2, fun))

data$foodstamps.num <- unlist(lapply(data$foodstamps, fun))
data$foodstamps2.num <- unlist(lapply(data$foodstamps2, fun))


data$foodstamps.diff <- data$foodstamps2.num - data$foodstamps.num
data$publichousing.diff <- data$publichousing2.num - data$publichousing.num
data$minimalwage.diff <- data$minimalwage2.num - data$minimalwage.num
data$aidpoor.diff <- data$aidpoor2.num - data$aidpoor.num
data$billionaires.diff <- data$billionaires2.num - data$billionaires.num
data$estatetax.diff <- data$estatetax2.num - data$estatetax.num
data$govred.diff <- data$govred2 - data$govred

data$govred.polreduce.l <- ifelse(data$govred.diff > 0 & data$govred < MID, 1, 0)
data$govred.polreduce.r <- ifelse(data$govred.diff < 0 & data$govred > MID, 1, 0)
data$govred.polreduce <- ifelse(data$govred.polreduce.l |  data$govred.polreduce.r, 1, 0)

data$foodstamps.polreduce.l <- ifelse(data$foodstamps.diff < 0 & data$foodstamps.num > MID, 1, 0)
data$foodstamps.polreduce.r <- ifelse(data$foodstamps.diff > 0 & data$foodstamps.num < MID, 1, 0) 
data$foodstamps.polreduce <- ifelse(data$foodstamps.polreduce.l |  data$foodstamps.polreduce.r, 1, 0)

data$publichousing.polreduce.l <- ifelse(data$publichousing.diff < 0 & data$publichousing.num > MID, 1, 0)
data$publichousing.polreduce.r <- ifelse(data$publichousing.diff > 0 & data$publichousing.num < MID, 1, 0) 
data$publichousing.polreduce <- ifelse(data$publichousing.polreduce.l |  data$publichousing.polreduce.r, 1, 0)

data$minimalwage.polreduce.l <- ifelse(data$minimalwage.diff < 0 & data$minimalwage.num > MID, 1, 0)
data$minimalwage.polreduce.r <- ifelse(data$minimalwage.diff > 0 & data$minimalwage.num < MID, 1, 0) 
data$minimalwage.polreduce <- ifelse(data$minimalwage.polreduce.l |  data$minimalwage.polreduce.r, 1, 0)

data$aidpoor.polreduce.l <- ifelse(data$aidpoor.diff < 0 & data$aidpoor.num > MID, 1, 0)
data$aidpoor.polreduce.r <- ifelse(data$aidpoor.diff > 0 & data$aidpoor.num < MID, 1, 0) 
data$aidpoor.polreduce <- ifelse(data$aidpoor.polreduce.l |  data$aidpoor.polreduce.r, 1, 0)

data$billionaires.polreduce.l <- ifelse(data$billionaires.diff < 0 & data$billionaires.num > MID, 1, 0)
data$billionaires.polreduce.r <- ifelse(data$billionaires.diff > 0 & data$billionaires.num < MID, 1, 0) 
data$billionaires.polreduce <- ifelse(data$billionaires.polreduce.l |  data$billionaires.polreduce.r, 1, 0)

data$estatetax.polreduce.l <- ifelse(data$estatetax.diff < 0 & data$estatetax.num > MID, 1, 0)
data$estatetax.polreduce.r <- ifelse(data$estatetax.diff > 0 & data$estatetax.num < MID, 1, 0) 
data$estatetax.polreduce <- ifelse(data$estatetax.polreduce.l |  data$estatetax.polreduce.r, 1, 0)


data$govred.polincrease.l <- ifelse(data$govred.diff > 0 & data$govred > MID, 1, 0)
data$govred.polincrease.r <- ifelse(data$govred.diff < 0 & data$govred < MID, 1, 0)
data$govred.polincrease <- ifelse(data$govred.polincrease.l |  data$govred.polincrease.r, 1, 0)

data$foodstamps.polincrease.l <- ifelse(data$foodstamps.diff < 0 & data$foodstamps.num < MID, 1, 0)
data$foodstamps.polincrease.r <- ifelse(data$foodstamps.diff > 0 & data$foodstamps.num > MID, 1, 0) 
data$foodstamps.polincrease <- ifelse(data$foodstamps.polincrease.l |  data$foodstamps.polincrease.r, 1, 0)

data$publichousing.polincrease.l <- ifelse(data$publichousing.diff < 0 & data$publichousing.num < MID, 1, 0)
data$publichousing.polincrease.r <- ifelse(data$publichousing.diff > 0 & data$publichousing.num > MID, 1, 0) 
data$publichousing.polincrease <- ifelse(data$publichousing.polincrease.l |  data$publichousing.polincrease.r, 1, 0)

data$minimalwage.polincrease.l <- ifelse(data$minimalwage.diff < 0 & data$minimalwage.num < MID, 1, 0)
data$minimalwage.polincrease.r <- ifelse(data$minimalwage.diff > 0 & data$minimalwage.num > MID, 1, 0) 
data$minimalwage.polincrease <- ifelse(data$minimalwage.polincrease.l |  data$minimalwage.polincrease.r, 1, 0)

data$aidpoor.polincrease.l <- ifelse(data$aidpoor.diff < 0 & data$aidpoor.num < MID, 1, 0)
data$aidpoor.polincrease.r <- ifelse(data$aidpoor.diff > 0 & data$aidpoor.num > MID, 1, 0) 
data$aidpoor.polincrease <- ifelse(data$aidpoor.polincrease.l |  data$aidpoor.polincrease.r, 1, 0)

data$billionaires.polincrease.l <- ifelse(data$billionaires.diff < 0 & data$billionaires.num < MID, 1, 0)
data$billionaires.polincrease.r <- ifelse(data$billionaires.diff > 0 & data$billionaires.num > MID, 1, 0) 
data$billionaires.polincrease <- ifelse(data$billionaires.polincrease.l |  data$billionaires.polincrease.r, 1, 0)

data$estatetax.polincrease.l <- ifelse(data$estatetax.diff < 0 & data$estatetax.num < MID, 1, 0)
data$estatetax.polincrease.r <- ifelse(data$estatetax.diff > 0 & data$estatetax.num > MID, 1, 0) 
data$estatetax.polincrease <- ifelse(data$estatetax.polincrease.l |  data$estatetax.polincrease.r, 1, 0)

##

data$language <- ifelse(data$language == "English ", "English", data$language)
data$democratic <- ifelse(data$demrep < MID, 'Yes', 'No')
data$independent <- ifelse(data$demrep == MID, 'Yes', 'No')
data$republican <- ifelse(data$demrep > MID, 'Yes', 'No')
data$liberal  <-  ifelse(data$libcons < MID, 'Yes', 'No')
data$conservative  <- ifelse(data$libcons > MID, 'Yes', 'No')
data$currentstate.lower <- tolower(data$currentstate)
data$candidate.all <- ifelse(data$candidate == 'Other', data$othercandidate, data$candidate)

data$candidate.all <- factor(data$candidate.all, 
                             levels=names(sort(table(data$candidate.all), 
                                               decreasing=TRUE)))

data$ineqprob <- factor(data$ineqprob, 
                        levels=c('Not a problem<br/>at all',
                                 'A small<br/>problem',
                                 'A problem',
                                 'A serious problem',
                                 'A very serious problem'))
