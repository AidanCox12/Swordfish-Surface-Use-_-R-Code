###WTF is this?
#SA.Prob_0.5m_adjust is a vector of proportions of time spent at or above 0.5m among sub adult swordfish, adjusted to account for missing data
#xbar_0.5m is the average of all the proportions of time spent at or above 0.5m for all individuals
#xdbar_0.5m is the grand mean of the average proportions of time spent at or above 0.5m for each age class
#Above_0.5 is a vector contatining the proportions of time spent at or above 0.5m for all individuals(not adjusted for NAs)

##Rainbow Histogram in ggplot2
n_bins <- length(ggplot2:::bin_breaks_width(range(myData), width = binwidth)$breaks) - 1L

ggplot() + geom_histogram(aes(x = df.100976$Depth), binwidth = binwidth, fill = rainbow(n_bins)) 

##qplot colored histogram
qplot(Depth, data = df.100976, fill = At_Surface, binwidth = 100)

##adjusted proportion of time at surface
head(prop.table(table(DepthX)))

##Two-Sample T-Test (Independent Groups)
# Ho = There is no difference between mean proportion of surface use between each age group
# Ha = At least one age group uses the surface a different amount
# Distribution = t-test
# Sides = Two
# Bonferroni Corrected Alpha = 0.5/3 = 0.01666667
# Variance = Unequal

# Conclusion - There is no significant difference between surface use at any age
