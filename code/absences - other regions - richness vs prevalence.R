data <- data.frame(richness=c(2,9,6,3,1,5), 
                   prop_without=c(.7391,.3860,.5910,.2031,.8171,.7649))

plot(data$richness, data$prop_without)

lm_data <- lm(prop_without ~ richness, data=data)
summary(lm_data)
