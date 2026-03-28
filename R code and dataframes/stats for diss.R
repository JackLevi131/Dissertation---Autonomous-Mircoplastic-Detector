m1 <-read.table("microplastic.txt",header = TRUE)
m2 <-read.table("9-2.txt",header = TRUE)

library(tidyr)
library(dplyr)

summary(m1)
m1$video <- as.factor(m1$video)
m1$case <- as.factor(m1$case) 
summary(m2)
m2$video <- as.factor(m2$video)
m2$case <- as.factor(m2$case) 

# detection t tests
##############################################
m1error <- m1$pred_total - m1$true_total
m2error <- m2$pred_total - m2$true_total

qqnorm(m1error)
qqline(m1error)
qqnorm(m2error)
qqline(m2error)

# normallity isnt great but it good enough, such a low sample size makes testing normality chargig. getting more data would be good but that will come after hand in

# model 1 detection
t.test(m1$pred_total-m1$true_total,mu=0)
# t = -4.983, p = 0.004 - depending on how you do it the answers are different

# model 2 detection
t.test(m2$true_total-m2$pred_total,mu=0)
# t = 0 , p = 1 

##############################################
# comparing models
t.test(m1$pred_total-m1$true_total,m2$pred_total-m2$true_total,paired = TRUE)
# t = -12.411, p = <0.001 - model 2 is significantly more accurate than model 1

##############################################
# comparing each class against each other
# beads
t.test(m1$pred_bead-m1$true_bead,mu=0)
# t = 1.713 p = 0.147
t.test(m2$pred_bead-m2$true_bead,mu=0)
# t = 3.859 p = 0.012

# fragments
t.test(m1$pred_frag-m1$true_frag,mu=0)
# t = -3.638 p = 0.015
t.test(m2$pred_frag-m2$true_frag,mu=0)
# t = -2.613 p = 0.048

#organic
t.test(m1$pred_org-m1$true_org,mu=0)
# t = 2.169 p = 0.082
t.test(m2$pred_org-m2$true_org,mu=0)
# t = -1.536 p = 0.185

##############################################
#graphs
library(ggplot2)
old_detect_df <- data.frame(video = m1$video, m1 = m1error, m2 = m2error)

# need to chnge shape of data
library(tidyr)
detect_df <- pivot_longer(old_detect_df, cols = c(m1, m2),names_to = "model",values_to = "error")

# graph for detection
detect_df <- detect_df %>%
  mutate(model = recode(model,
                        "m1" = "Model 1",
                        "m2" = "Model 2"))

ggplot(detect_df, aes(x = model, y = error)) +
  geom_jitter(width = 0.05, size = 2, colour = "#dd9d41") +
  stat_summary(fun = mean, geom = "point", size = 10, shape = 15, colour = "black") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Detection Error", x = "Model") +
  theme_classic()


# comparing models - dont
# class error 
m1$bead_error <- m1$pred_bead - m1$true_bead
m2$bead_error <- m2$pred_bead - m2$true_bead

m1$frag_error <- m1$pred_frag - m1$true_frag
m2$frag_error <- m2$pred_frag - m2$true_frag

m1$org_error <- m1$pred_org - m1$true_org
m2$org_error <- m2$pred_org - m2$true_org

old_class_df <- data.frame(
  video = m1$video,
  m1_bead = m1$bead_error,
  m2_bead = m2$bead_error,
  m1_frag = m1$frag_error,
  m2_frag = m2$frag_error,
  m1_org  = m1$org_error,
  m2_org  = m2$org_error)

class_df <- pivot_longer(old_class_df,cols = -video,names_to = "variable", values_to = "error")

class_df$model <- ifelse(grepl("m1", class_df$variable),"Model 1", "Model 2")

class_df$class <- ifelse(grepl("bead", class_df$variable), "Bead",
                           ifelse(grepl("frag", class_df$variable), "Fragment", "Organic"))

ggplot(class_df,
       aes(x = model, y = error)) +
  geom_jitter(width = 0.05, size = 2, colour = "#dd9d41") +
  stat_summary(fun = mean,
               geom = "point",
               size = 4,
               shape = 15,
               colour = "black") +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               width = 0.2) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  facet_wrap(~ class) +
  labs(y = "Class Error",
       x = "Model") +
  theme_classic()

p1 <- ggplot(detect_df, aes(x = model, y = error)) +
  geom_jitter(width = 0.05, size = 2, colour = "#dd9d41") +
  stat_summary(fun = mean, geom = "point", size = 10, shape = 15, colour = "black") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Detection Error", x = "Model") +
  theme_classic()

p2 <- ggplot(class_df,
             aes(x = model, y = error)) +
  geom_jitter(width = 0.05, size = 2, colour = "#dd9d41") +
  stat_summary(fun = mean,
               geom = "point",
               size = 4,
               shape = 15,
               colour = "black") +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               width = 0.2) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  facet_wrap(~ class) +
  labs(y = "Class Error",
       x = "Model") +
  theme_classic()

library (patchwork)
p1 + p2 +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(size = 20, face = "bold", colour = "#000000"),
    plot.tag.position = c(0.02, 0.98) 
  )
citation("dplyr")
citation("tidyr")
citation("ggplot2")

f1<- read.table("f1t.txt", header = TRUE)

f1_long <- f1 %>%
  pivot_longer(
    cols = -origin,
    names_to = "metric",
    values_to = "f1_score"
  )
cb_palette <- c(
  "bead" = "#1142f2",
  "fragment" = "#d99e01",
  "organic" = "#119d73",
  "average score" = "darkgrey"
)

f1_avg <- f1_long %>%
  filter(metric == "average")

f1_classes <- f1_long %>%
  filter(metric != "average")

ggplot() +
  
  geom_col(
    data = f1_avg,
    aes(x = origin, y = f1_score),
    fill = "grey80",
    width = 0.8,
    na.rm = TRUE
  ) +
  
  geom_col(
    data = f1_classes,
    aes(x = origin, y = f1_score, fill = metric),
    position = position_dodge(width = 0.6),
    width = 0.5,
    na.rm = TRUE
  ) +
  
  scale_fill_manual(values = c(
    "bead" = "#73956F",
    "fragment" = "#227C9D",
    "organic" = "#854D27"
  )) +
  
  coord_cartesian(ylim = c(0.7, 1)) +
  
  labs(
    x = "Model and detection stream",
    y = "F1 Score",
    fill = "Class"
  ) +
  
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "top"
  )

