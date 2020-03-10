## Plot TPS curve with different smoothing parameters on Monthly Net Earnings against Monthly orders

library(ggplot2)
library(dplyr)
library(fields)


# m = 2

earnings.tps1 = Tps(as.numeric(dd.m$month), dd.m$m.net.earnings, m=2, method="GCV")

earnings.se1 = predictSE(earnings.tps1)

t1 = dd.m %>% mutate(smooth_1 = earnings.tps1$fitted.values, 
                LB = earnings.tps1$fitted.values - qt(0.975, earnings.tps1$eff.df)*earnings.se1, 
                UB = earnings.tps1$fitted.values + qt(0.975, earnings.tps1$eff.df)*earnings.se1) %>%
      ggplot(aes(month, m.net.earnings)) +
      theme_bw() +
      labs(title = "m = 2") + 
      labs(x = "Month", y = "Earnings ($)", caption="Source: Poshmark (@Stylesforu)") +
      geom_point(size = 2, alpha = .5, color = "darkgrey") +
      geom_line(aes(month, smooth_1), color = "brown4",size = 1) +
      geom_line(aes(month, LB), color = "brown4", size = 1, linetype = "dashed", alpha = .5) +
      geom_line(aes(month, UB), color = "brown4", size = 1, linetype = "dashed", alpha = .5)

# m = 3

earnings.tps2 = Tps(as.numeric(dd.m$month), dd.m$m.net.earnings, m=3, method="GCV")

earnings.se2 = predictSE(earnings.tps2)


t2 = dd.m %>% mutate(smooth_2 = earnings.tps2$fitted.values, 
                     LB = earnings.tps1$fitted.values - qt(0.975, earnings.tps2$eff.df)*earnings.se2, 
                     UB = earnings.tps1$fitted.values + qt(0.975, earnings.tps2$eff.df)*earnings.se2) %>%
      ggplot(aes(month, m.net.earnings)) +
      theme_bw() +
      labs(title = "m = 3") + 
      labs(x = "Month", y = "Earnings ($)", caption="Source: Poshmark (@Stylesforu)") +
      geom_point(size = 2, alpha = .5, color = "darkgrey") +
      geom_line(aes(month, smooth_2), color = "brown4",size = 1) +
      geom_line(aes(month, LB), color = "brown4", size = 1, linetype = "dashed", alpha = .5) +
      geom_line(aes(month, UB), color = "brown4", size = 1, linetype = "dashed", alpha = .5)

# m = 4

earnings.tps3 = Tps(as.numeric(dd.m$month), dd.m$m.net.earnings, m=4, method="GCV")

earnings.se3 = predictSE(earnings.tps3)


t3 = dd.m %>% mutate(smooth_3 = earnings.tps3$fitted.values, 
                     LB = earnings.tps3$fitted.values - qt(0.975, earnings.tps3$eff.df)*earnings.se3, 
                     UB = earnings.tps3$fitted.values + qt(0.975, earnings.tps3$eff.df)*earnings.se3) %>%
      ggplot(aes(month, m.net.earnings)) +
      theme_bw() +
      labs(title = "m = 4") + 
      labs(x = "Month", y = "Earnings ($)", caption="Source: Poshmark (@Stylesforu)") +
      geom_point(size = 2, alpha = .5, color = "darkgrey") +
      geom_line(aes(month, smooth_3), color = "brown4",size = 1) +
      geom_line(aes(month, LB), color = "brown4", size = 1, linetype = "dashed", alpha = .5) +
      geom_line(aes(month, UB), color = "brown4", size = 1, linetype = "dashed", alpha = .5)

# m = 5

earnings.tps4 = Tps(as.numeric(dd.m$month), dd.m$m.net.earnings, m=5, method="GCV")

earnings.se4 = predictSE(earnings.tps4)


t4 = dd.m %>% mutate(smooth_4 = earnings.tps4$fitted.values, 
                     LB = earnings.tps4$fitted.values - qt(0.975, earnings.tps4$eff.df)*earnings.se4, 
                     UB = earnings.tps4$fitted.values + qt(0.975, earnings.tps4$eff.df)*earnings.se4) %>%
  ggplot(aes(month, m.net.earnings)) +
  theme_bw() +
  labs(title = "m = 5") + 
  labs(x = "Month", y = "Earnings ($)", caption="Source: Poshmark (@Stylesforu)") +
  geom_point(size = 2, alpha = .5, color = "darkgrey") +
  geom_line(aes(month, smooth_4), color = "brown4",size = 1) +
  geom_line(aes(month, LB), color = "brown4", size = 1, linetype = "dashed", alpha = .5) +
  geom_line(aes(month, UB), color = "brown4", size = 1, linetype = "dashed", alpha = .5)


multiplot(t1, t3, t2, t4, cols = 2) # Plots the four graphs into one plane






# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}