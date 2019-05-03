library(dygraphs)

AirPassengers

dygraph(AirPassengers, main = "Airline Passengers / Month") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y", label = "Passengers (Thousands)") %>%
  dyOptions(includeZero = TRUE, 
            axisLineColor = "navy", 
            gridLineColor = "lightblue")

lungDeaths <- cbind(ldeaths, mdeaths, fdeaths)
dygraph(lungDeaths, main = "Deaths from Lung Disease (UK)") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"))

dygraph(lungDeaths, main = "Deaths from Lung Disease (UK)") %>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))
