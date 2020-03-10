## Create LOESS fitted values

library(ggplot2)
library(dplyr)
library('fANCOVA')

p.loess1 = loess.as(dd.m$month, dd.m$m.net.earnings, degree=1, criterion = c("aicc", "gcv")[1], plot = FALSE)
aicc_value1 = p.loess1$pars$span
fit1.p = predict(loess(m.net.earnings ~ as.numeric(month), degree=1, span = aicc_value1, data=dd.m), se=TRUE)
p.loess2 = loess.as(dd.m$month, dd.m$m.net.earnings, degree=2, criterion = c("aicc", "gcv")[1], plot = FALSE)
aicc_value2 = p.loess2$pars$span
fit2.p = predict(loess(m.net.earnings ~ as.numeric(month), degree=2, span = aicc_value2, data=dd.m), se=TRUE)


## Plot LOESS curve (degree 1 and 2) on Monthly Net Earnings against Monthly orders

dd.m %>% mutate(smooth_1 = fit1.p$fit, smooth_2 = fit2.p$fit) %>%
  ggplot(aes(month, m.net.earnings)) +
  theme_bw() +
  labs(title = "Monthly Net Earnings", subtitle = "Monthly Net Earnings: Jan 2015 - Mar 2019") + 
  labs(x = "Month", y = "Earnings ($)", caption="Source: Poshmark (@Stylesforu)") +
  geom_point(size = 2, alpha = .5, color = "darkgrey") +
  geom_line(aes(month, smooth_1, color = "smooth_1"), size = 1) +
  geom_line(aes(month, smooth_2, color = "smooth_2"), size = 1) +
  scale_color_discrete(name = "Degree of Local Polynomial", labels = c("D = 1", "D = 2"))



## Plot LOESS curves (degree 1) with 95% confidence bands

dd.m %>% mutate(smooth_1 = fit1.p$fit, LB = fit1.p$fit - qt(0.975, fit1.p$df)*fit1.p$se, UB = fit1.p$fit + qt(0.975, fit1.p$df)*fit1.p$se) %>%
  ggplot(aes(month, m.net.earnings)) +
  theme_bw() +
  labs(title = "LOESS Curve", subtitle = "Degree 1") + 
  labs(x = "Month", y = "Earnings ($)", caption="Source: Poshmark (@Stylesforu)") +
  geom_point(size = 2, alpha = .5, color = "darkgrey") +
  geom_line(aes(month, smooth_1), color = "brown4",size = 1) +
  geom_line(aes(month, LB), color = "brown4", size = 1, linetype = "dashed", alpha = .5) +
  geom_line(aes(month, UB), color = "brown4", size = 1, linetype = "dashed", alpha = .5)

## Plot LOESS curves (degree 2) with 95% confidence bands

dd.m %>% mutate(smooth_2 = fit2.p$fit, LB = fit2.p$fit - qt(0.975, fit2.p$df)*fit2.p$se, UB = fit2.p$fit + qt(0.975, fit2.p$df)*fit2.p$se) %>%
  ggplot(aes(month, m.net.earnings)) +
  theme_bw() +
  labs(title = "LOESS Curve", subtitle = "Degree 2") + 
  labs(x = "Month", y = "Earnings ($)", caption="Source: Poshmark (@Stylesforu)") +
  geom_point(size = 2, alpha = .5, color = "darkgrey") +
  geom_line(aes(month, smooth_2), color = "brown4",size = 1) +
  geom_line(aes(month, LB), color = "brown4", size = 1, linetype = "dashed", alpha = .5) +
  geom_line(aes(month, UB), color = "brown4", size = 1, linetype = "dashed", alpha = .5)

## Plot LOESS curves (degree 2) with different span values


# span = 0.2

plx1 = predict(loess(m.net.earnings ~ as.numeric(month), degree=2, span = 0.2, data=dd.m), se=TRUE)

p1 = dd.m %>% mutate(smooth_2 = plx1$fit, LB = plx1$fit - qt(0.975, plx1$df)*plx1$se, UB = plx1$fit + qt(0.975, plx1$df)*plx1$se) %>%
  ggplot(aes(month, m.net.earnings)) +
  theme_bw() +
  labs(title = "Span = 0.2") + 
  labs(x = "Month", y = "Earnings ($)", caption="Source: Poshmark (@Stylesforu)") +
  geom_point(size = 2, alpha = .5, color = "darkgrey") +
  geom_line(aes(month, smooth_2), color = "brown4",size = 1) +
  geom_line(aes(month, LB), color = "brown4", size = 1, linetype = "dashed", alpha = .5) +
  geom_line(aes(month, UB), color = "brown4", size = 1, linetype = "dashed", alpha = .5)

# span = 0.3

plx2 = predict(loess(m.net.earnings ~ as.numeric(month), degree=2, span = 0.3, data=dd.m), se=TRUE)

p2 = dd.m %>% mutate(smooth_2 = plx2$fit, LB = plx2$fit - qt(0.975, plx2$df)*plx2$se, UB = plx2$fit + qt(0.975, plx2$df)*plx2$se) %>%
  ggplot(aes(month, m.net.earnings)) +
  theme_bw() +
  labs(title = "Span = 0.3") + 
  labs(x = "Month", y = "Earnings ($)", caption="Source: Poshmark (@Stylesforu)") +
  geom_point(size = 2, alpha = .5, color = "darkgrey") +
  geom_line(aes(month, smooth_2), color = "brown4",size = 1) +
  geom_line(aes(month, LB), color = "brown4", size = 1, linetype = "dashed", alpha = .5) +
  geom_line(aes(month, UB), color = "brown4", size = 1, linetype = "dashed", alpha = .5)

# span = 0.4

plx3 = predict(loess(m.net.earnings ~ as.numeric(month), degree=2, span = 0.4, data=dd.m), se=TRUE)

p3 = dd.m %>% mutate(smooth_2 = plx3$fit, LB = plx3$fit - qt(0.975, plx3$df)*plx3$se, UB = plx3$fit + qt(0.975, plx3$df)*plx3$se) %>%
  ggplot(aes(month, m.net.earnings)) +
  theme_bw() +
  labs(title = "Span = 0.4") + 
  labs(x = "Month", y = "Earnings ($)", caption="Source: Poshmark (@Stylesforu)") +
  geom_point(size = 2, alpha = .5, color = "darkgrey") +
  geom_line(aes(month, smooth_2), color = "brown4",size = 1) +
  geom_line(aes(month, LB), color = "brown4", size = 1, linetype = "dashed", alpha = .5) +
  geom_line(aes(month, UB), color = "brown4", size = 1, linetype = "dashed", alpha = .5)


# span = 0.5

plx4 = predict(loess(m.net.earnings ~ as.numeric(month), degree=2, span = 0.5, data=dd.m), se=TRUE)

p4 = dd.m %>% mutate(smooth_2 = plx4$fit, LB = plx4$fit - qt(0.975, plx4$df)*plx4$se, UB = plx4$fit + qt(0.975, plx4$df)*plx4$se) %>%
  ggplot(aes(month, m.net.earnings)) +
  theme_bw() +
  labs(title = "Span = 0.5") + 
  labs(x = "Month", y = "Earnings ($)", caption="Source: Poshmark (@Stylesforu)") +
  geom_point(size = 2, alpha = .5, color = "darkgrey") +
  geom_line(aes(month, smooth_2), color = "brown4",size = 1) +
  geom_line(aes(month, LB), color = "brown4", size = 1, linetype = "dashed", alpha = .5) +
  geom_line(aes(month, UB), color = "brown4", size = 1, linetype = "dashed", alpha = .5)


multiplot(p1, p3, p2, p4, cols = 2) # Plots the four graphs into one plane







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