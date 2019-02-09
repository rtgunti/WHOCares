plot1 <- ggplot(data = FNDDS, mapping = aes(x = GDP, y = AverageTemperature, colour = Country))
plot1 <- plot1 + geom_point() + scale_x_log10() + labs(x = "GDP (log 10 scale)", y = "Temperature", title = "GDP (in USD) vs AverageTemperature")
plot1 <- plot1 + theme_stata() 

plot2 <- ggplot(data = FNDDS, mapping = aes(x = Population, y = AverageTemperature, color = Country))
plot2 <- plot2 + geom_point() + scale_x_log10() + labs(x = "Population (log 10 scale)", y = "Temperature", title = "Population vs AverageTemperature")
plot2 <- plot2 + theme_calc()

plot3 <- ggplot(data = FNDDS, mapping = aes(x = Emissions, y = AverageTemperature, color = Country))
plot3 <- plot3 + geom_point() + scale_x_log10() + labs(x = "CO2 Emissions in Metric Tonnes (log 10 scale)", y = "Temperature",title = "CO2 Emissions vs AverageTemperature")
plot3 <- plot3 + theme_economist() 

# cat to print spaces between the plots
print(plot1)
cat("\n")
cat("\n")
cat("\n")
cat("\n")
print(plot2)
cat("\n")
cat("\n")
cat("\n")
cat("\n")
print(plot3)