##Load libraries into R.

library(tidyverse)
library(gghighlight)
library(scales)

##Load the data into R.
f1wins <- read.csv("alltimef1winners.csv", stringsAsFactors = FALSE)

#Scale percentage

scaleFUN <- function(x) sprintf("%.0f", x)


#Filter the data to 10

f1winsv2 <- f1wins %>% filter(Wins > 24.5)

#Code to highlight colour in bar

f1winsv2 <- f1winsv2 %>% mutate( ToHighlight = ifelse(Wins=="99", "yes", "no"))

#Create the barplot visualisation

barplot <- ggplot(f1winsv2, aes(x = reorder(Driver, +Wins), y = Wins, fill= ToHighlight )) + coord_flip() + theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.position = "none", axis.text.x = element_blank()) + geom_bar(stat = "identity") + scale_fill_manual(values = c("yes"="#00D2BE", "no"="#FF8700", guide=TRUE)) + geom_text(aes(label=Wins), vjust=0.3, hjust=-0.3) + scale_y_continuous(expand = expand_scale(mult = c(0, .1)))
       

barplot


#Before creating another bar chart, we need to filter the data to focus on 

f1winsv3 <- filter(f1wins, Wins >= 40)


#Here's the manual way to sort out the F1 driver names in ggplot.


#Lovely. Time to create another barplot

  barplot2 <- ggplot(f1winsv3, aes(x=reorder(Driver,+Wins/Starts),y= Wins/Starts, fill=Driver)) + geom_col() + coord_flip() + scale_fill_manual(values = c("#FF8700", "#FF8700", "#00D2BE", "#FF8700", "#FF8700")) + geom_bar(stat = "identity") + theme(panel.grid = element_blank(), panel.border = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), legend.position = "none", axis.text.x = element_blank()) + scale_y_continuous(labels= scales::percent, expand = expand_scale(mult = c(0.01, .06))) + geom_text(aes(label=Winpc, hjust=1.2, vjust=0.5))

  barplot2

