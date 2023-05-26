
# DATA
beta = read.csv("analysis/derived/b_coeff/beta_njs.csv")

beta

# PLOT

(B_plot = ggplot(data = beta) +
    geom_line(aes(x = as.factor(time_start), y = beta, group = 1) , color = "#53868B", linewidth = 1) +
    geom_point(aes(x = as.factor(time_start), y = beta), shape= 22, fill = "#53868B", color = "white", size = 3 ) +
    labs(title = "NJS Survey -  B-coefficient index ",
         subtitle = "") +
    scale_y_continuous(name = "B") +
    scale_x_discrete(name = "Year") +
    theme_minimal_hgrid() +
    theme(
      legend.position = "top",
      axis.text = element_text(size = 8),
      plot.subtitle=element_text(face="italic"),
      plot.margin = unit(c(1,1,1,1), units = , "cm")))

ggsave("analysis/figures/a_plot_njs.png", bg = "white")
