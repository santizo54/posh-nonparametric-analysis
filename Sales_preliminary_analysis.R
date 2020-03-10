## Plot Monthly Net Earnings against Monthly orders

library(dplyr)
library(ggplot2)
library(ggpmisc)

dd.m %>% 
  ggplot(aes(as.POSIXct(month), m.net.earnings)) +
  theme_bw() +
  labs(title = "Monthly Net Earnings", subtitle = "Monthly Net Earnings: Jan 2015 - Mar 2019") + 
  labs(x = "Month", y = "Earnings ($)", caption="Source: Poshmark (@Stylesforu)") +
  geom_point(size = 3, alpha = .35, color = "black") +
  geom_line(linetype = "dotted", alpha = .75) + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", ignore_threshold = 0,
            vjust = -0.4, hjust = -0.1, x.label.fmt = "%b %Y") +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "text", colour = "blue", ignore_threshold = 0,
             vjust = 0.4, hjust = -0.1, x.label.fmt = "%b %Y")
  
  


