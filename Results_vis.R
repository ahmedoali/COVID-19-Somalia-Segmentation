# Results visualisations --------------------------------------------------

library(reshape2)
means <- data.frame(profile_km_rc, stringsAsFactors = FALSE)[c(7:16,42:44),] %>%
  rownames_to_column() %>%
  rename(variables = rowname) %>%
  melt(id.vars = "variables", variable.name = "Segments", value.name = "Mean") %>%
  mutate(Mean = round(Mean, 2))




p <- means %>%
  ggplot(aes(variables, Mean, group = Segments, color = Segments)) +
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  scale_x_discrete(limits = colnames(profiling_data)[c(9:18,44:46)]) +
  labs(x = NULL, y = "Standardized means") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

# Interactive plot
library(plotly)

ggplotly(p, tooltip = c("variables", "Mean")) %>%
  layout(legend = list(orientation = "h", y = 1.2))
