require(ggpattern)
require(gridExtra)
library(grid)
require(GGally)
require(RColorBrewer)

# ggplot theme for Nature style
text_size = 8
update_geom_defaults("text", list(size = text_size))
theme_nature <- function () { 
  theme_classic(base_size = text_size, base_family = 'sans') %+replace% 
    theme(
      axis.text = element_text(size = text_size),
      axis.title = element_text(size = text_size),
      plot.title = element_text(size = text_size + 2, face = "bold", hjust = 0.5, margin = ggplot2::margin(0, 0, 5, 0))
    )
}

theme_bw2 <- function () { 
  theme_bw(base_size = text_size, base_family = 'sans') %+replace% 
    theme(
      axis.text = element_text(size = text_size),
      axis.title = element_text(size = text_size),
      plot.title = element_text(size = text_size + 2, face = "bold", hjust = 0, margin = ggplot2::margin(0, 0, 5, 0))
    )
}

# color palettes
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# save plots
save_plot <- function(pl, pdf_file = NULL, tikz_file = NULL, w = 8, h = 4) {
  print(pl)
  if (!is.null(pdf_file)) {
    ggsave(pdf_file, width = w / cm(1), height = h / cm(1)) 
  }
  if (!is.null(tikz_file)) {
    tikz(tikz_file, width = w / cm(1), height = h / cm(1))
    print(pl)
    dev.off()
  } 
}
