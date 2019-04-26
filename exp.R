if(.Platform$OS.type == "unix") {
  THISDIR <- '~/Dropbox/msr-ineq/R/'
} else {
  THISDIR <- 'C:/Users/stbaliet/Dropbox/msr-ineq/R/'
}
setwd(THISDIR)
source('init.R')


## essays <- data[c("treatment", "demrep", "govred", "govred2", "essay")]
## essays$essay <- gsub('\\,', ',', essays$essay)

## write.table(essays, paste0(THISDIR, 'essays'))

## feeds <- data[c("treatment", "demrep", "govred", "govred2", "feedback")]
## write.table(feeds, paste0(THISDIR, 'feedbacks'))


## TODO To compute Pooled SD and CI


## #############
## 
## mysm <- summarySE(pr, c("dse.sub.curr.mean"), c("com","round"), na.rm=TRUE)
## colnames(summaryDiv.other2) <- c("com", "round", "N.other", "dse.sub.curr.mean",
##                                  "sd.other", "sum.other", "se.other", "ci.other")
## 
## summaryDiv.all2 <- merge(summaryDiv, summaryDiv.other2, by=c("com", "round"))
## summaryDiv.all2$diff <- summaryDiv.all2$dse.sub.curr - summaryDiv.all2$d.sub.current
## 
## 
## top <-  with(summaryDiv.all2, ((N -1) * sd^2) + ((N.other -1) * sd.other^2))
## bottom <-  with(summaryDiv.all2, (N + N.other - 2))
## 
## 
## summaryDiv.all2$diff.sp <- sqrt(top/bottom)
## 
## 
## summaryDiv.all2$diff.ci <- with(summaryDiv.all2, qt(.975, (N + N.other - 2)) *
##                                 diff.sp * sqrt((1 / N) + (1 / N.other)))
## 
## 
## p <- ggplot(summaryDiv.all2, aes(round, diff, color=com))
## p <- p + geom_hline(size = 2, linetype="dashed", alpha = 0.5, yintercept=0)
## p <- p + geom_point(size = 5)
## p <- p + geom_line(size=2, alpha=0.5)
## p <- p + geom_errorbar(aes(ymin = diff - diff.ci, ymax = diff + diff.ci), alpha = 0.5)
## p <- p + xlab("Rounds") + ylab("Avg. Difference Own vs Other Sessions")
## p <- p + scale_x_discrete(breaks=c(1, 10, 20,30))
## p <- p + ylim(-0.035,0.1)
## p <- p + scale_color_discrete(breaks=c(0,1), labels=c("non-Com","Com"), name = "Relative Diversity")
## p <- p + myThemeMod + theme(legend.title = element_text(vjust=3, size=24, face="bold"))
## p
## ###############

####################################################################################


## For analysis related to the main questions.
mydata <- data[data$weird == 0,]
#############################################

## Stance

ggplot(mydata, aes(stance)) + geom_histogram(aes(fill=party), position="dodge")


ggplot(mydata, aes(stance2)) + geom_histogram(aes(fill=party), position="dodge") + facet_grid(~treatment)

## Polarization Analysis.

## Reduction.

polarCols <- c("govred.polreduce", "estatetax.polreduce", 
               "billionaires.polreduce", "aidpoor.polreduce", 
               "minimalwage.polreduce", "publichousing.polreduce", "foodstamps.polreduce")

md1 <- mydata[,c("player", "treatment", "democratic", polarCols)]
md1.melted <- melt(md1, id.vars=c("player", "treatment", "democratic"))

mysm <- summarySE(md1.melted, "value", "variable")
mysm$variable  <- gsub('.polreduce', '', mysm$variable)

ggplot(mysm, aes(variable, value, fill=variable)) +
    geom_bar(stat="identity", position="dodge", color="white") +
    geom_errorbar(aes(ymin=value-ci, ymax=value+ci, position="dodge")) +
    xlab('') + ylab('Decrease in Polarization') +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1)
          )
ggsave('poldecrease.jpg')

## Reduction by treatment.

mysm <- summarySE(md1.melted, "value", c("variable", "treatment"))
mysm$variable  <- gsub('.polreduce', '', mysm$variable)

ggplot(mysm, aes(treatment, value, fill=variable, group=variable)) +
    geom_bar(stat="identity", position="dodge", color="white") +
    geom_errorbar(aes(ymin=value-ci, ymax=value+ci, group=variable), position="dodge") +
    xlab('') + ylab('Decrease in Polarization') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('poldecrease_treatment.jpg', width=12)

## Increase.

polarCols <- c("govred.polincrease", "estatetax.polincrease", 
               "billionaires.polincrease", "aidpoor.polincrease", 
               "minimalwage.polincrease", "publichousing.polincrease", "foodstamps.polincrease")


md1 <- mydata[,c("player", "treatment", "democratic", polarCols)]
md1.melted <- melt(md1, id.vars=c("player", "treatment", "democratic"))
#colnames(md1.melted) <- c("player", "treatment", "democratic", "variable", "value")

mysm <- summarySE(md1.melted, "value", "variable")
mysm$variable  <- gsub('.polincrease', '', mysm$variable)

ggplot(mysm, aes(variable, value, fill=variable)) +
    geom_bar(stat="identity", position="dodge", color="white") +
    geom_errorbar(aes(ymin=value-ci, ymax=value+ci, position="dodge")) +
    xlab('') + ylab('Increase in Polarization') +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1)
          )
ggsave('polincrease.jpg')

## Increase by Treatment.

mysm <- summarySE(md1.melted, "value", c("variable", "treatment"))
mysm$variable  <- gsub('.polincrease', '', mysm$variable)

ggplot(mysm, aes(treatment, value, fill=variable, group=variable)) +
    geom_bar(stat="identity", position="dodge", color="white") +
    geom_errorbar(aes(ymin=value-ci, ymax=value+ci, group=variable), position="dodge") +
    xlab('') + ylab('Increase in Polarization') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)
          )
ggsave('polincrease_treatment.jpg', width=12)


####################

## Redistribution Support

redisCols <- c("estatetax", 
               "billionaires", "aidpoor", 
               "minimalwage", "publichousing", "foodstamps")
redisCols <- paste0(redisCols, ".num")
redisCols <- c("govred", redisCols)

md1 <- mydata[,c("player", "treatment", "democratic", redisCols)]
md1.melted <- melt(md1, id.vars=c("player", "treatment", "democratic"))
#colnames(md1.melted) <- c("player", "treatment", "democratic", "variable", "value")
md1.melted$t <- 1

redisCols2 <- c("estatetax2",
               "billionaires2", "aidpoor2", 
               "minimalwage2", "publichousing2","foodstamps2")
redisCols2 <- paste0(redisCols2, ".num")
redisCols2 <- c("govred2", redisCols2)

md2 <- mydata[,c("player", "treatment", "democratic", redisCols2)]
colnames(md2) <- c("player", "treatment", "democratic", redisCols)
md2.melted <- melt(md2, id.vars=c("player", "treatment", "democratic"))
# colnames(md2.melted) <- c("player", "treatment", "democratic", "variable", "value")
md2.melted$t <- 2


md1B <- mydata[,c("player", "treatment", "democratic", redisCols)]
md1B.melted <- melt(md1B, id.vars=c("player", "treatment", "democratic"))
colnames(md1B.melted) <- c("player", "treatment", "democratic", "variable", "test")

md2B <- mydata[,c("player", redisCols2)]
colnames(md2B) <- c("player", redisCols)
md2B.melted <- melt(md2B, id.vars=c("player"))
colnames(md2B.melted) <- c("player", "variable", "retest")


mdB <- merge(md1B.melted, md2B.melted, by=c("player", "variable"))
mdB$diff <- mdB$retest - mdB$test

mdB$variable <- unlist(lapply(mdB$variable, function(v){
  if (v == "govred") return("gov red")
  if (v == "estatetax.num") return("estate")
  if (v == "billionaires.num") return("bill")
  if (v == "aidpoor.num") return("aidpoor")
  if (v == "minimalwage.num") return("min wage")
  if (v == "publichousing.num") return("housing")
  if (v == "foodstamps.num") return("food st.")
  stop('WTF')
}))

mysm <- summarySE(mdB, "diff", c("variable"))

ggplot(mysm, aes(variable, diff, fill=variable)) +
  geom_bar(stat="identity", position="dodge", color="white") +
  geom_errorbar(aes(ymin=diff-ci, ymax=diff+ci, position="dodge")) +
  xlab('') + ylab('Support') +
  ggtitle('All Data')
ggsave(filename = "diff_alldata.jpg")


mysm <- summarySE(mdB, "diff", c("variable", "treatment"))

ggplot(mysm, aes(treatment, diff, fill=variable)) +
  geom_bar(stat="identity", position="dodge", color="white") +
  geom_errorbar(aes(ymin=diff-ci, ymax=diff+ci, position="dodge")) +
  xlab('') + ylab('Support') +
  facet_grid(~variable) + theme(legend.position = "none")
ggsave(filename = "diff_treatment.jpg", width=15)


mysm <- summarySE(mdB, "diff", c("variable", "democratic"))

ggplot(mysm, aes(democratic, diff, fill=variable)) +
  geom_bar(stat="identity", position="dodge", color="white") +
  geom_errorbar(aes(ymin=diff-ci, ymax=diff+ci, position="dodge")) +
  xlab('Democrats') + ylab('Support') +
  facet_grid(~variable) + theme(legend.position = "none")
ggsave(filename = "diff_democrat.jpg", width=15)

mysm <- summarySE(mdB, "diff", c("variable", "democratic", "treatment"))

ggplot(mysm, aes(democratic, diff, fill=variable)) +
  geom_bar(stat="identity", position="dodge", color="white") +
  geom_errorbar(aes(ymin=diff-ci, ymax=diff+ci, position="dodge")) +
  xlab('') + ylab('Support') +
  facet_grid(treatment~variable) + theme(legend.position = "none")


md <- rbind(md1.melted, md2.melted)
md$variable <- unlist(lapply(md$variable, function(v){
  if (v == "govred") return("gov red")
  if (v == "estatetax.num") return("estate")
  if (v == "billionaires.num") return("bill")
  if (v == "aidpoor.num") return("aidpoor")
  if (v == "minimalwage.num") return("min wage")
  if (v == "publichousing.num") return("housing")
  if (v == "foodstamps.num") return("food st.")
  stop('WTF')
}))


mysm <- summarySE(md, "value", c("variable", "t"))
mysm$t <- as.factor(mysm$t)


ggplot(mysm, aes(t, value, fill=t)) +
    geom_bar(stat="identity", position="dodge", color="white") +
    geom_errorbar(aes(ymin=value-ci, ymax=value+ci, position="dodge")) +
    xlab('') + ylab('Support') +
    facet_grid(~variable) + theme(legend.position="none") +
    ggtitle('All Data')
ggsave(filename = "alldata.jpg")


mysm <- summarySE(md[md$treatment == "convince",], "value", c("variable", "t"))
mysm$t <- as.factor(mysm$t)

ggplot(mysm, aes(t, value, fill=t)) +
  geom_bar(stat="identity", position="dodge", color="white") +
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci, position="dodge")) +
  xlab('') + ylab('Support') +
  facet_grid(~variable) + theme(legend.position="none") +
  ggtitle('Treatment "Convince"')
ggsave(filename = "treatment_convince.jpg")

mysm <- summarySE(md[md$treatment == "explain",], "value", c("variable", "t"))
mysm$t <- as.factor(mysm$t)

ggplot(mysm, aes(t, value, fill=t)) +
  geom_bar(stat="identity", position="dodge", color="white") +
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci, position="dodge")) +
  xlab('') + ylab('Support') +
  facet_grid(~variable) + theme(legend.position="none") +
  ggtitle('Treatment "Explain"')
ggsave(filename = "treatment_explain.jpg")


mysm <- summarySE(md[md$democratic=="No",], "value", c("variable", "t"))
mysm$t <- as.factor(mysm$t)

ggplot(mysm, aes(t, value, fill=t)) +
  geom_bar(stat="identity", position="dodge", color="white") +
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci, position="dodge")) +
  xlab('') + ylab('Support') +
  facet_grid(~variable) + theme(legend.position="none") +
  ggtitle('Republican Only')
ggsave(filename = "republican.jpg")


mysm <- summarySE(md[md$democratic=="Yes",], "value", c("variable", "t"))
mysm$t <- as.factor(mysm$t)

ggplot(mysm, aes(t, value, fill=t)) +
  geom_bar(stat="identity", position="dodge", color="white") +
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci, position="dodge")) +
  xlab('') + ylab('Support') +
  facet_grid(~variable) + theme(legend.position="none") +
  ggtitle('Democrats Only')
ggsave(filename = "democrats.jpg")


######################

summary(data$govred)
summary(data$govred2)


summarySE(data, "govred.diff", "democratic")
summarySE(data, "estatetax.diff", "democratic")
summarySE(data, "billionaires.diff", "democratic")
summarySE(data, "aidpoor.diff", "democratic")
summarySE(data, "minimalwage.diff", "democratic")
summarySE(data, "publichousing.diff", "democratic")
summarySE(data, "foodstamps.diff", "democratic")


summarySE(data, "govred.diff", "treatment")
summarySE(data, "estatetax.diff", "treatment")
summarySE(data, "billionaires.diff", "treatment")
summarySE(data, "aidpoor.diff", "treatment")
summarySE(data, "minimalwage.diff", "treatment")
summarySE(data, "publichousing.diff", "treatment")
summarySE(data, "foodstamps.diff", "treatment")


###############################

summarySE(data, "govred", "democratic")
summarySE(data, "govred2", "democratic")

summarySE(data, "estatetax.num", "democratic")
summarySE(data, "estatetax2.num", "democratic")

summarySE(data, "billionaires.num", "democratic")
summarySE(data, "billionaires2.num", "democratic")

summarySE(data, "aidpoor.num", "democratic")
summarySE(data, "aidpoor2.num", "democratic")

summarySE(data, "minimalwage.num", "democratic")
summarySE(data, "minimalwage2.num", "democratic")

summarySE(data, "publichousing.num", "democratic")
summarySE(data, "publichousing2.num", "democratic")

summarySE(data, "foodstamps.num", "democratic")
summarySE(data, "foodstamps2.num", "democratic")


summarySE(data, "govred", "treatment")
summarySE(data, "govred2", "treatment")




##############################

## General Plots.


ggplot(data, aes(color)) +
    geom_histogram(stat="count", position="dodge") +
    theme(legend.position = "none") +
    xlab('') + ylab('') + coord_flip()
ggsave('color.jpg')

ggplot(data, aes(vacation)) +
    geom_histogram(stat="count", position="dodge") +
    theme(legend.position = "none") +
    xlab('') + ylab('') + coord_flip()
ggsave('vacation.jpg')

ggplot(data, aes(food)) +
    geom_histogram(stat="count", position="dodge") +
    theme(legend.position = "none") +
    xlab('') + ylab('') + coord_flip()
ggsave('food.jpg')

cols <- paste0("tvshows.", seq(0,5))
tvshows <- data[, cols]
tvshows <- c(tvshows[,1],tvshows[,2],tvshows[,3],tvshows[,4],tvshows[,5])
tvshows <- tvshows[tvshows != "0"]

tmp <- as.data.frame(table(tolower(tvshows)))

ggplot(tmp, aes(Var1, Freq)) +
    geom_bar(stat="identity", position="dodge") +
    theme(legend.position = "none") +
    xlab('') + ylab('') + coord_flip()
ggsave('tvshows.jpg')


ggplot(data[!is.na(data$sportfollow),], aes(sportfollow)) +
    geom_histogram(stat="count", position="dodge") +
    theme(legend.position = "none") +
    xlab('') + ylab('') + coord_flip()
ggsave('sportfollow.jpg')


ggplot(data[!is.na(data$bestteam),], aes(bestteam)) +
    geom_histogram(stat="count", position="dodge") +
    theme(legend.position = "none") +
    xlab('') + ylab('') + coord_flip()
ggsave('bestteam.jpg')

ggplot(data[!is.na(data$bestteam),], aes(bestteam)) +
    geom_histogram(stat="count", position="dodge") +
    theme(legend.position = "none") +
    xlab('') + ylab('') + coord_flip()
ggsave('bestteam.jpg')


ggplot(data[!is.na(data$college),], aes(college)) +
    geom_histogram(stat="count", position="dodge") +
    theme(legend.position = "none") +
    xlab('') + ylab('') + coord_flip()
ggsave('college.jpg')

ggplot(data, aes(siblings)) +
    geom_histogram(stat="count", position="dodge") +
    theme(legend.position = "none") +
    xlab('') + ylab('')
ggsave('siblings.jpg')

ggplot(data, aes(children)) +
    geom_histogram(stat="count", position="dodge") +
    theme(legend.position = "none") +
    xlab('') + ylab('')
ggsave('children.jpg')


ggplot(data, aes(education)) +
    geom_histogram(stat="count", position="dodge") +
    theme(legend.position = "none") +
    xlab('') + ylab('')
ggsave('education.jpg')

ggplot(data, aes(income, fill=income)) +
    geom_histogram(stat="count", position="dodge") +
    theme(legend.position = "none") +
    xlab('') + ylab('')
ggsave('income.jpg')

ggplot(data, aes(gender, fill=gender)) +
    geom_histogram(stat="count", width=0.4) +
    theme(legend.position = "none") +
    xlab('') + ylab('')
ggsave('gender.jpg', width=5)


ggplot(data, aes(age, fill=gender)) +
    geom_histogram(stat="count", position="dodge") +
    theme(legend.position = "none") +
    xlab('') + ylab('')

ggsave('gender_age.jpg')


ggplot(data[data$initials != FALSE & data$initials != TRUE,],
       aes(initials, fill=initials)) +
    geom_histogram(stat="count", position="dodge") +
    theme(legend.position = "none") +
    xlab('') + ylab('')
ggsave('initials.jpg')


## Times

ggplot(data, aes(reorder(player, essayTime), essayTime)) +
    geom_point() +
    theme(legend.position = "none") +
    xlab('') + ylab('Essay Time in Sec.') +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1)
          )
ggsave('essayTime.jpg')

ggplot(data, aes(reorder(player, overallTime), overallTime/60)) +
    geom_point() +
    geom_hline(yintercept=median(data$overallTime)/60) +
    theme(legend.position = "none") +
    xlab('') + ylab('Overall Time in Min.') +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1)
          )
ggsave('overallTime.jpg')

## Words.

ggplot(data, aes(reorder(player, essayWords), essayWords)) +
    geom_point() +
    geom_hline(yintercept=median(data$essayWords)) +
    geom_hline(yintercept=250, color="red") +
    theme(legend.position = "none") +
    xlab('') + ylab('Essay Words.') +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1)
          )
ggsave('essayWords.jpg')


##

usa <- map_data("usa")
states <- map_data("state")

statesCounts <- as.data.frame(table(data$currentstate.lower))
colnames(statesCounts) <- c("region", "freq")
statesCounts$region <- as.character(statesCounts$region)
for (s in unique(states$region)) {
    if (!(s %in% unique(statesCounts$region))) {
        statesCounts <- rbind(statesCounts, data.frame(region=s, freq=0))
    }
}

gg <- ggplot()
gg <- gg + geom_map(data=states, map=states,
                    aes(x=long, y=lat, map_id=region),
                    fill="#ffffff", color="#ffffff", size=0.15)
gg <- gg + geom_map(data=statesCounts, map=states,
                    aes(fill=freq, map_id=region),
                    color="#ffffff", size=0.15)
gg <- gg + scale_fill_continuous(low='thistle2', high='darkred', 
                                 guide='colorbar')
gg <- gg + labs(x=NULL, y=NULL)
gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45) 
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(panel.background = element_blank())
gg <- gg + theme(axis.ticks = element_blank())
gg <- gg + theme(axis.text = element_blank())
gg

ggsave('statescurrent.jpg')

##

ggplot(data, aes(followpol, fill=followpol)) +
    geom_histogram(stat="count", position="dodge") +
    theme(legend.position = "none") +
    xlab('Follow US Politics') + ylab('')

ggsave('follow_politics.jpg')


ggplot(data, aes(demrep, fill=demrep)) +
    geom_histogram(stat="count", position="dodge") +
    theme(legend.position = "none") +
    xlab('Democrat vs Republic') + ylab('')
ggsave('demrep.jpg')

ggplot(data, aes(libcons, fill=libcons)) +
    geom_histogram(stat="count", position="dodge") +
    theme(legend.position = "none") +
    xlab('Liberal vs Conservative') + ylab('')

ggsave('libcons.jpg')


ggplot(data, aes(candidate.all, fill=libcons)) +
    geom_histogram(stat="count", position="dodge") +
    theme(legend.position = "none") +
    xlab('Candidate Supported 2016') + ylab('') +
    coord_flip()

ggsave('candidate.jpg')

ggplot(data, aes(ineqprob, fill=ineqprob)) +
    geom_histogram(stat="count", position="dodge") +
    theme(legend.position = "none") +
    xlab('Inequality as a Problem') + ylab('') +
    coord_flip()

ggsave('ineqprob.jpg')


ineqsources <- c("talent", "workhard", "easierjobs",
                 "globalization", "techchange", "finance",
                 "lobbies", "minorities", "restricted_edu",
                 "policies", "luck", "family")

md <- data[,c("player", ineqsources)]

md.melted <- melt(md, id.vars="player")

mysm <- summarySE(md.melted, "value", "variable")
mysm$group <- c(1,1,1,2,2,2,3,3,3,3,4,4)

ggplot(mysm, aes(variable, value, fill=as.factor(group))) +
    geom_bar(stat="identity") + coord_flip() + theme(legend.position="none")


ggsave('ineqsources.jpg')

#####################################################3

## Matches simulation.

matches <- read.csv('matches.csv')
matches$self <- ifelse(matches$PlayerId == matches$MatchId, 1, 0)
matches$sameparty <- factor(ifelse(matches$Party == matches$MatchParty, 1, 0))
matches$partypair  <- apply(matches, 1, function(row) {
    a <- row["Party"]
    b <- row["MatchParty"]
    if ((a == "D" && b == "R") ||
        (a == "R" && b == "D")) {
        return("DR")
    }
    if ((a == "D" && b == "I") ||
        (a == "I" && b == "D")) {
        return("DI")
    }
    if ((a == "R" && b == "I") ||
        (a == "I" && b == "R")) {
        return("RI")
    }
    return(paste0(a,b))
})

summarySE(matches[matches$self == 0,], "Score", "sameparty")

mysm <- summarySE(matches[matches$self == 0,], "Score", "partypair")

ggplot(mysm, aes(partypair, Score, fill=partypair)) +
    geom_bar(stat="identity", position="dodge") +
    geom_errorbar(aes(ymin=Score-ci, ymax=Score+ci))


ggplot(matches[matches$self == 1,], aes(reorder(PlayerId, Score), Score, color=Party)) +
    geom_point(size=5) + ylab('Self Similarity') + xlab('') +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1)
          )

ggplot(matches[matches$self == 0,], aes(reorder(PlayerId, Score),  Score, color=Party)) +
    geom_point(size=5) + ylab('Self Similarity') + xlab('') +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1)
          )

ggplot(matches[matches$self == 0,], aes(reorder(PlayerId, Score), Score)) +
    geom_point(size=5, aes(color=MatchParty)) +
    ylab('Self Similarity') + xlab('') +
    facet_grid(MatchParty~Party) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1)
          )
