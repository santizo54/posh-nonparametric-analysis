## Plot bar chart of sales by states

library(scales)

s = ggplot(aes(x=states),data = data.frame(states))

s + geom_bar(aes(y = (..count..)/sum(..count..)), width=0.7, fill="steelblue", data=data.frame(states)) +  
  theme_minimal() +
  scale_y_continuous(labels = percent) +
  labs(x = "US States", y = "Percentage of Sales")