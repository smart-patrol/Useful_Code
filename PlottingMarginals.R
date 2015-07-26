library(ggplot2)
library(ggExtra)

frm = read.csv("C:\\Users\\Cfrenzel\\Documents\\Code\\Practice\\tips.csv")

plot_center = ggplot(frm
                     ,aes(x = total_bill
                         ,y = tip)) +
  geom_point() +
  geom_smooth(method = "lm")

ggMarginal(plot_center, type = "histogram")