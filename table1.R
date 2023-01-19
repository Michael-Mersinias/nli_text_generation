library(tidyverse)
library(xtable)

d = read_csv("scarecrow_table1_tidy.csv")
unique(d$p)

d = filter(d, p %in% c(.96, .4))

a = NULL
for (i in c("ENT", "NEU", "CON")) {
  for (j in c(.4, .96)) { 
    for (k in c("redundant", "off_prompt", "self_contradiction", "incoherent")) {
      x = filter(d, NLI_Label == i,
                     p == j)
      z = (cor.test(as.numeric(x[, k][[1]]), x$NLI_Prob, method="spearman"))
      a = bind_rows(a, c(i, j, k, z$p.value, z$estimate))
    }
  }
}

a = data.frame(a)
names(a) = c("NLI", "parameter p", "error", "pval", "rho")
a$rho = as.numeric(a$rho)
a$text = paste0(ifelse(as.numeric(a$pval) < .05, "*", ""), round(a$rho, 2))
a$text = paste0(ifelse(as.numeric(a$pval) < .01, "**", ""), a$text)
a$`parameter p` = paste("randomness:", a$`parameter p`)

a$error = gsub("redundant", "RD", a$error)
a$error = gsub("off_prompt", "OP", a$error)
a$error = gsub("self_contradiction", "SC", a$error)
a$error = gsub("incoherent", "IN", a$error)

ggplot(a, aes(x=NLI, y=error, fill=rho, label=text)) +
  facet_grid(. ~ `parameter p`) + 
  geom_tile() +
  theme_classic(10) +
  theme(legend.position="none") + 
  geom_text(size=3) + 
  scale_fill_gradient2(
    low = 'green', mid = 'white', high = 'red',
    limits=c(-1, 1),
    midpoint = 0, guide = 'colourbar', aesthetics = 'fill'
  ) +
  xlab("") + ylab("")
ggsave("heatmap_table1.png", width=4, height=3)
