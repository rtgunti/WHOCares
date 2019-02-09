inds <- FNDDS %>% filter(Country=="IND")
inds <- select (inds,"Year","AverageTemperature","Emissions","GDP", "Population")

# Plot the average temperature graph over the years
p1 <- ggplot(data = inds, mapping = aes(x = Year, y = AverageTemperature))
p1 <- p1 + geom_point() + geom_smooth(method = "lm", color = "firebrick", se = FALSE, size = 2) + labs(x = "Year", y = "AverageTemperature")
ind_plot1 <- p1 + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())

# Plot the GDP growth graph over the years
p2 <- ggplot(data = inds, mapping = aes(x = Year, y = GDP))
p2 <- p2 + geom_point() + labs(x = "Year", y = "GDP")
ind_plot2 <- p2 + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())

# Plot the Co2 Emissions graph over the years
p3 <- ggplot(data = inds, mapping = aes(x = Year, y = Emissions))
p3 <- p3 + geom_point() + labs(x = "Year", y = "Emissions")
ind_plot3 <- p3 + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())

# Plot the Population graph over the years
p4 <- ggplot(data = inds, mapping = aes(x = Year, y = Population))
p4 <- p4 + geom_point() + labs(x = "Year", y = "Population")
ind_plot4 <- p4 + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())

# Arrange the plots in a grid manner
#grid.arrange(ind_plot1, ind_plot2, ncol=2)
grid.arrange(ind_plot1, ind_plot2, ind_plot3, ind_plot4, ncol=4)
# cowplot::plot_grid(ind_plot3, ind_plot4, labels="AUTO")
# print(ind_plot1) # FIX THE ISSUE WITH cowplot

# ggplot(data = inds) +
#   geom_histogram(mapping = aes(x = AverageTemperature), binwidth = 0.15)