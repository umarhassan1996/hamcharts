#Load the libraries

library(tidyverse)
library(gghighlight)
library(scales)
library(lubridate)

#Load dataset
ham <- read.csv("hamf120072021.csv", stringsAsFactors = FALSE)


#Scale percentage

scaleFUN <- function(x) sprintf("%.0f", x)


#Create the bar chart

bar <- ggplot(ham, aes(x=year,y=Avgpodiumspergp, fill = Constructor)) + scale_fill_manual(values = c("#E0610E", "#00D2BE")) + geom_bar(stat = "identity") + theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank(), panel.border = element_blank(), panel.grid = element_blank(), axis.line = element_line(size = 1, colour = "black")) + scale_y_continuous(labels=scaleFUN, expand = expand_scale(mult = c(0.001, .06))) + scale_x_continuous(expand = expand_scale(mult = c(0.01, .05))) + geom_text(aes(label=Avgpodiumspergp, hjust=0.5, vjust=1.1))

bar

#Create the lineplot

lineplot <- ggplot(ham, aes(x=Year, y=Avgpodiumspergp)) + geom_line(size = 1, colour = "black") + theme(legend.position = "none", axis.title.x = element_blank(), panel.border = element_blank(), panel.grid = element_blank(), axis.line = element_line(size = 1, colour = "black")) + labs(y="Avg. podiums per GP for season")
lineplot

#Ham wins barplot 2

bar2 <- ggplot(ham, aes(x=Year, y=W, fill = Constructor)) + geom_bar(stat = "identity") + scale_fill_manual(values = c("#E0610E", "#00D2BE")) + geom_bar(stat = "identity") + theme(legend.position = "none", axis.title.x = element_blank(), panel.border = element_blank(), panel.grid = element_blank(), axis.line = element_line(size = 1, colour = "black")) + scale_y_continuous(labels=scaleFUN, expand = expand_scale(mult = c(0.001, .06))) + scale_x_continuous(expand = expand_scale(mult = c(0.01, .05))) + geom_text(aes(label=W, hjust=0.5, vjust=1.1)) + labs(y="Wins")

bar2

#Ham wins lineplot 2

lineplot2 <- ggplot(ham, aes(x=Year, y=Avgwinspergp)) + geom_line(size = 1, colour = "black") + theme(legend.position = "none", axis.title.x = element_blank(), panel.border = element_blank(), panel.grid = element_blank(), axis.line = element_line(size = 1, colour = "black")) + labs(y="Average win per GP for season")

lineplot2


#Ham points finishes barplot

bar3 <- ggplot(ham, aes(x=Year, y=PF, fill = Constructor)) + geom_bar(stat = "identity") + scale_fill_manual(values = c("#E0610E", "#00D2BE")) + geom_bar(stat = "identity") + theme(legend.position = "none", axis.title.x = element_blank(), panel.border = element_blank(), panel.grid = element_blank(), axis.line = element_line(size = 1, colour = "black")) + scale_y_continuous(labels=scaleFUN, expand = expand_scale(mult = c(0.001, .06))) + scale_x_continuous(expand = expand_scale(mult = c(0.01, .05))) + geom_text(aes(label=PF, hjust=0.5, vjust=1.1)) + labs(y="Points Finishes")

bar3

#Ham avg points finish per GP.

lineplot3 <- ggplot(ham, aes(x=Year, y=Avgpointsfinishpergp)) + geom_line(size = 1, colour = "black") + theme(legend.position = "none", axis.title.x = element_blank(), panel.border = element_blank(), panel.grid = element_blank(), axis.line = element_line(size = 1, colour = "black")) + labs(y="Avg. pts finish per GP season")

lineplot3


#Time to create the third (not final) barplot.

bar4 <- ggplot(ham, aes(x=reorder(Year,W/RS), y=W/RS, fill=Constructor)) + scale_fill_manual(values = c("#FF8700", "#00D2BE"))  + geom_bar(stat = "identity") + coord_flip() + labs(x="Year", y="Win percentage per season") + theme(legend.position = "none", axis.title.y = element_blank(), panel.border = element_blank(), panel.grid = element_blank(), axis.line = element_line(size = 1, colour = "black")) + scale_y_continuous(label = scales::percent, expand = expand_scale(mult = c(0.01, .05))) + geom_text(aes(label=Win, hjust=-0.1, vjust=0.25)) + labs(y="Win percentage per season")

bar4

#Penultimate bar plot incoming!

bar5 <- ggplot(ham, aes(x=Year,y=FR, fill=Constructor)) + scale_fill_manual(values = c("#E0610E", "#00D2BE")) + geom_bar(stat = "identity") + theme(panel.grid = element_blank(), panel.border = element_blank(), axis.ticks = element_blank(), legend.position = "none", axis.line = element_line(size=1, color="black")) + scale_y_continuous(expand = expand_scale(mult = c(0.01, .06))) + geom_text(aes(label=FR, hjust=0.5, vjust=-0.5)) + labs(y = "Total top two starts on grid per season")

bar5


