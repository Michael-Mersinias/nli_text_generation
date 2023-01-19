library(tidyverse)
library(ggrepel)

d = read_csv("error_types_annotation_results.csv")
cbb <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

group_by(d, global_id) %>%
  summarise(n=n())

d.sum = group_by(d, nli_tag, p_value, label, prompt_id) %>%
  summarise(avg.label.span.pct = mean(label_span_percentage)) %>%
  group_by(nli_tag, p_value, label)   %>%
  summarise(m = mean(avg.label.span.pct),
            se = sd(avg.label.span.pct)/sqrt(n()),
            l95 = m - 1.96 * se,
            u95 = m + 1.96 * se) 

d$nli_tag = factor(d$nli_tag, levels=c("Vanilla GPTJ",
                                       "NLI GPJ - NEU",
                                       "NLI GPJ - ENT",
                                       "NLI GPJ - CON"))

a.errors = NULL
tags = c("Vanila GPTJ", "NLI GPJ - NEU", "NLI GPJ - ENT", "NLI GPJ - CON")
for (j in unique(d$p_value)) {
    for (i in unique(d$label) ) {
    x = filter(d, label == i, p_value==j) %>%
      group_by(nli_tag, prompt_id) %>%
      summarise(m=mean(label_span_percentage))
  l = lm(data= x,
       m ~  nli_tag)
  for (k in 2:4) {
    a.errors = rbind(a.errors, cbind(i, j,  summary(l)$coefficients[,4][k], levels(d$nli_tag)[k]))
  }
  print(i)
  print(j)
  print(summary(l))
  }
}

a.errors = data.frame(a.errors)
names(a.errors) = c("label", "p_value", "p", "nli_tag")
a.errors$p = as.numeric(a.errors$p)
a.errors$stars = ifelse(a.errors$p < .05, "*", "")
a.errors$stars = ifelse(a.errors$p < .01, "**", a.errors$stars)

#########################################
d.rating = read_csv("holistic_ratings_annotation_results.csv") %>%
  group_by(nli_tag, p_value, prompt_id) %>%
  summarise(avg.rating = mean(rating))

d.rating$nli_tag = factor(d.rating$nli_tag, levels=c("Vanilla GPTJ",
                                       "NLI GPJ - NEU",
                                       "NLI GPJ - ENT",
                                       "NLI GPJ - CON"))

for (j in unique(d.rating$p_value)) {
    x = filter(d.rating, p_value==j) 
    l = lm(data= x,
           avg.rating ~  nli_tag)
    print(j)
    print(summary(l))
}


d.sum.rating =  d.rating %>%
  group_by(nli_tag, p_value)   %>%
  summarise(m = mean(avg.rating),
            se = sd(avg.rating)/sqrt(n()),
            l95 = m - 1.96 * se,
            u95 = m + 1.96 * se) %>%
  mutate(label="Rating")

for (i in unique(d$label) ) {
  for (j in unique(d$p_value)) {
    x = filter(d, label == i, p_value==j) %>%
      group_by(nli_tag, prompt_id) %>%
      summarise(m=mean(label_span_percentage))
    l = lm(data= x,
           m ~  nli_tag)
    print(i)
    print(j)
    print(summary(l))
  }
}

a.errors$p_value = as.numeric(a.errors$p_value)

d.sum = left_join(d.sum, a.errors)
d.sum$text = d.sum$nli_tag
d.sum$text = ifelse(grepl("CON", d.sum$text), "CON", d.sum$text)
d.sum$text = ifelse(grepl("ENT", d.sum$text), "ENT", d.sum$text)
d.sum$text = ifelse(grepl("NEU", d.sum$text), "NEU", d.sum$text)
d.sum$text = ifelse(d.sum$text == "Vanilla GPTJ", "control", d.sum$text)

d.sum$text = ifelse(d.sum$text != "control", paste0(d.sum$text, d.sum$stars), d.sum$text)
d.sum$p_value = paste("param p =", d.sum$p_value)
d.sum$label = ifelse(d.sum$label == "Self-Contradiction", "Self-Cont", d.sum$label)

d.sum$nli_tag = factor(d.sum$nli_tag, levels=c("Vanilla GPTJ", "NLI GPJ - NEU", "NLI GPJ - ENT", "NLI GPJ - CON"))

ggplot(d.sum, aes(x="Off-prompt", y=m, ymax=u95, ymin=l95, group=nli_tag, fill=nli_tag)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(position=position_dodge(width=.9)) +
  facet_grid(label ~ p_value) + 
  theme_classic() + 
  #theme(legend.position=c(.85, .8)) +
  theme(legend.position="none", axis.text.y = element_blank()) + 
  coord_flip() + 
  geom_text(position=position_dodge(width=.9), 
            data=d.sum, aes(x="Off-prompt", y=u95 + .05, group=nli_tag, colour=nli_tag, label=text),
            size=2) +
  scale_fill_manual(values =cbb) + #values=c("lightblue", "darkblue", "red", "gray")) +
  scale_colour_manual(values=cbb) + #("lightblue", "darkblue", "red", "gray")) +
  xlab("") + ylab("mean proportion error span for each type")
ggsave("errors.png", width=3, height=3.5)


group_by(d, nli_tag, p_value, label) %>%
  summarise(avg.num.spans = mean(num_of_spans)) %>%
  spread(nli_tag, avg.num.spans)  %>%
  arrange(p_value, label)
##################################@##############
d.sum = d.sum.rating

d.sum$text = as.character(d.sum$nli_tag)
d.sum$text = ifelse(grepl("CON", d.sum$text), "CON", d.sum$text)
d.sum$text = ifelse(grepl("ENT", d.sum$text), "ENT", d.sum$text)
d.sum$text = ifelse(grepl("NEU", d.sum$text), "NEU", d.sum$text)
d.sum$text = ifelse(d.sum$text == "Vanilla GPTJ", "control", d.sum$text)
d.sum$p_value = paste("param p =", d.sum$p_value)
ggplot(d.sum, aes(x="Off-prompt", y=m, ymax=u95, ymin=l95, group=nli_tag, fill=nli_tag)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(position=position_dodge(width=.9)) +
  facet_grid(. ~ p_value) + 
  theme_classic() + 
  #theme(legend.position=c(.85, .8)) +
  theme(legend.position="none", axis.text.y = element_blank()) + 
  coord_flip() + 
  geom_text(position=position_dodge(width=.9), 
            data=d.sum, aes(x="Off-prompt", y=u95 + .5, group=nli_tag, colour=nli_tag, label=text),
            size=2) +
  scale_fill_manual(values =cbb) + #values=c("lightblue", "darkblue", "red", "gray")) +
  scale_colour_manual(values=cbb) + #("lightblue", "darkblue", "red", "gray")) +
  xlab("") + ylab("avg. rating (1-5)") +
  ylim(0, 5)
ggsave("ratings.png", width=4, height=1.5)

###################################################
d = read_csv("ratings_results_01092022.csv")

group_by(d, nli_tag, p_value) %>%
  summarise(avg.rating = mean(rating)) %>%
  spread(p_value, avg.rating) 

group_by(d, nli_tag, p_value, label) %>%
  summarise(avg.num.spans = mean(num_of_spans)) %>%
  spread(nli_tag, avg.num.spans)  %>%
  arrange(p_value, label)

###################################################




group_by(d, nli_tag, labels) %>%
  summarise(avg.num.spans =mean(num_of_spans)) %>%
  spread(labels, avg.num.spans)

filter(d, is.na(rating) == F)  %>%
  group_by(nli_tag) %>% 
  summarise(m=mean(rating))


#####################################################
scarecrow1 = tibble(p=c(rep(.96, 4), rep(.40, 4)),
       error=c("Off Prompt", "Self Contradictory", "Incoherent", "Redundant", 
               "Off Prompt", "Self Contradictory", "Incoherent", "Redundant"),
       value=c(17.93, 3.44, 7.58, 10.34, 6.89, 3.44, 0.86, 38.79)
           ) %>%
  mutate(`parameter p`=as.factor(p),
         value=value/100)
ggplot(scarecrow1, aes(x=error, y=value, group=`parameter p`, fill=`parameter p`)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  theme_classic() + 
  theme(legend.position=c(.9, .8)) + 
  scale_fill_manual(values =cbb) + #values=c("lightblue", "darkblue", "red", "gray")) +
  scale_colour_manual(values=cbb) +
  ylim(0, .5) + 
  ylab("proportion of error type") + 
  xlab("error types") #("lightblue", "darkblue", "red", "gray")) +
ggsave("scarecrow_errortypes.png", width=4, height=2.3)  

#########################################3
scarecrow2 = tibble(p=c(rep("param p: .96", 3), rep("param p: .40", 3)),
           value=c(1.37, 86.20, 12.41, 12.93, 83.62, 3.44),
           nli_tag=rep(c("entailment", "neutral", "contradiction"), 2)
)


# Compute the position of labels
scarecrow2 <- scarecrow2 %>% 
  group_by(p) %>%
  arrange(desc(nli_tag)) %>%
  mutate(prop = value / sum(scarecrow2$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(scarecrow2, aes(x="", y=prop, fill=nli_tag)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void(14) + 
  theme(legend.position="none") +
  geom_text_repel(aes(y = ypos, label = paste0(nli_tag, ":\n", as.character(value))), color = "white", size=2) +
  scale_fill_manual(values=cbb) +
  facet_grid(. ~ p) +
  xlab("") + ylab("") 
ggsave("scarecrow_pie.png", width=4, height=4)  

scarecrow2 %>%
  select(p, value, nli_tag) %>%
  spread(nli_tag, value) %>%
  xtable() %>%
  print(row.names = F)
