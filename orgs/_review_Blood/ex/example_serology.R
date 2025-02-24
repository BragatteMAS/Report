library(tidyverse)
library(RColorBrewer)

pal <- brewer.pal(8, "Set2")
prev <- read.csv("/Users/bragatte/Documents/GitHub/Report/Blood/ex/prev.csv")
dfplot <- read.csv("/Users/bragatte/Documents/GitHub/Report/Blood/ex/dfplot.csv")
prev$truncated.date <- as.Date(prev$truncated.date)
dfplot$DT_SIN_PRI <- as.Date(dfplot$DT_SIN_PRI)

dfplot.bounds <- dfplot |> filter(DT_SIN_PRI %in% c(min(dfplot$DT_SIN_PRI), max(dfplot$DT_SIN_PRI)))
dfplot.bounds <- dfplot.bounds |> mutate(x.bias = ifelse(DT_SIN_PRI == as.Date("2023-11-01"), -15, 15))
dfplot.numbers <- data.frame(DT_SIN_PRI = prev$truncated.date, location = prev$location, n.events = prev$q3)
dates.plt <- seq(as.Date("2023-05-01"), as.Date("2024-07-01"), by = "m")
dates.label <- strftime(dates.plt, format = "%b/%Y")

plt <- ggplot(prev, aes(x = truncated.date, y = 100*q3, fill = location)) +  geom_errorbar(aes(ymin = 100*q1, ymax = 100*q5), position = "dodge2", width=30, linewidth = 0.1) +
  geom_crossbar(aes(ymin = 100*q2, ymax = 100*q4), position = "dodge2", linewidth = 0.1,width=30) +
  geom_line(data = dfplot, aes(x = DT_SIN_PRI, y = 100*n.events, colour = location), inherit.aes = FALSE) + facet_wrap(~ event.type, scale = "free_y", ncol=1) + theme_minimal() +
  labs(x = "Date of symptom onset", y = "Confirmed Dengue cases per inhabitant since 2000 (%)") +
  theme(strip.text = element_text(face="bold", size=9), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  scale_x_continuous(breaks = dates.plt, labels = dates.label) + scale_colour_manual("", values = pal) +
  scale_fill_manual("", values = pal) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) + facet_wrap(~ location, scale = "free_y") +
  geom_point(data = dfplot.bounds, aes(x = DT_SIN_PRI, y = 100*n.events, colour = location), size = 3) +
  geom_text_repel(data = dfplot.bounds, aes(x = DT_SIN_PRI, y = 100*n.events, label = paste0(format(100*n.events,digits=2), "%")),
                  nudge_y = 1) +
  geom_text_repel(data = dfplot.numbers, aes(x = DT_SIN_PRI, y = 100*n.events, label = paste0(format(100*n.events,digits=2), "%")), nudge_x = -90)

ggsave(plt, file = "figs/sinan_reporting_rate_simplified_twomonths_IgM.pdf", width = 10, height = 6, dpi = 600)
