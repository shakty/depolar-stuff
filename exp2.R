
detach("package:ggplot2", unload=TRUE)
detach("package:scales", unload=TRUE, character.only=TRUE)
unloadNamespace("ggplot2")
unloadNamespace("scales")


library(DataExplorer)

plot_str(data)

plot_missing(data)

plot_histogram(data)

plot_correlation(data, type = 'continuous','Review.Date')


## Demo 1

cols.demo1 <- c("birthdayDate", "birthday", "birthmonth",
                "birthyear", "zodiac", "age", "agegroup",
                "race.0", "race.1", "language", "otherlanguage", "totlanguage",
                "initials", "gender", "othergender", 
                "eyes", "othereyes")
md <- data[,cols.demo1]

create_report(md)

## Demo 2





