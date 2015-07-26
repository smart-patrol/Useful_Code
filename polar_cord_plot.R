rm(list = ls(all = TRUE))

library(RODBC)
library(dplyr)
library(ggplot2)
library(reshape2)
library(XLConnect)


library(ggthemr)

ggthemr('flat')
#devtools::install_github('ggthemr', 'cttobin')


read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

dat=read.excel()

str(dat)

dat$per <- dat$Percent * 100


ggplot(dat, aes(x = spec, y = per, fill = spec)) +
  geom_bar(stat = "identity") +
  coord_polar() + labs(x="", y="" ) +
  geom_text(aes(label = paste0(round(per, 1), "%")),
            size = 16, hjust = 1) +
  theme(legend.position="none",   text = element_text(size=28),
       axis.text.y = element_blank(),
   axis.ticks = element_blank()      ) 
 # scale_fill_brewer(palette = 1) 
