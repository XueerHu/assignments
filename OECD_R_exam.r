
library(ggplot2)
library(dplyr)

oecd <- read.csv2("oecddata.csv")

head(oecd)
oecd

graph1 <- filter(oecd, Year %in% c('2014'))
ggplot(graph1, aes(x = Cost, y = LE)) + geom_point() + geom_smooth(method=lm)

graph2 <- filter(oecd, Country %in% c('Belgium', 'United States', 'Japan', 'Denmark', 'Chile'))%>% select( Country, Year, Cost)
ggplot(graph2, aes(x = Year , y = Cost, col=Country)) + geom_line()

eff <- graph1 %>%
    filter(!is.na(LE)) %>%
    mutate(Ratio = Cost / LE) %>%
    mutate(Score = min(Ratio) / Ratio) %>%
    arrange(Score)
eff

eff.bar <- ggplot(eff, aes(x = reorder(Country,Score), y = Score, fill= Country)) +
        geom_bar(stat = "identity") +
        theme(text = element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1))+
        scale_fill_manual(values = c(rep("blue",21),rep("red",1),rep("blue",12))) +
        guides(fill=FALSE) +
        xlab("Country") +
        ylab("Efficiency scores according to Xueer Hu & Wenqian Lu")
 
eff.bar


