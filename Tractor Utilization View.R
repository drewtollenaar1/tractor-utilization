library(readxl)
library(tidyverse)
library(gridExtra)
df <- read_excel("C:\\Users\\tollenaard\\Downloads\\2020.06_Tractor Utilization (1).xlsm", sheet = "tbl_2020.06")
miles <- data.frame(df)

pcs <- c("878", "888", "478", "861", "823", "805", "439")

miles <- miles %>% 
  mutate(pc = as.character(Profit.Center))
miles

daycab <- miles %>% 
  filter(Type == "Day Cab" & pc %in% pcs)

#class(miles$pc)
dp <- daycab %>% 
  ggplot(aes(pc, Total.Miles, group = pc)) +
  geom_boxplot(width = 0.5) +
  geom_point() +
  stat_boxplot(geom = 'errorbar', width = 0.25) +
  ggtitle("Miles by Day Cab Tractor")

dp
dp1 <- 2034



sleeper <- miles %>% 
  filter(Type == "Sleeper" & pc %in% pcs)

sp <- sleeper %>% 
  ggplot(aes(pc, Total.Miles, group = pc)) +
  geom_boxplot(width = 0.5) +
  geom_point() +
  stat_boxplot(geom = 'errorbar', width = 0.25) +
  ggtitle("Miles by Sleeper Tractor")
sp



both <- miles %>% 
  filter(pc %in% pcs)
bp <- both %>% 
  ggplot(aes(pc, Total.Miles, group = pc)) +
  geom_boxplot(width = 0.5) +
  #geom_point(aes(color = Type)) +
  geom_jitter(aes(color = Type), width = 0.05, alpha = 0.9) +
  stat_boxplot(geom = 'errorbar', width = 0.25) +
  ggtitle("Miles by Tractor Type") +
  theme(legend.position = "bottom")
bp


densp <- both %>% 
  ggplot(aes(Total.Miles, fill = Type)) +
  geom_density(alpha = 0.5) +
  theme(legend.position = "bottom") +
  ggtitle("Miles Distribution by Type")
  
densp


# up <- 2
# low <- -2
# 
# covp <- both %>% 
#   ggplot(aes(pc, Z.Score, group = pc)) +
#   geom_boxplot(width = 0.5) +
#   #geom_point(aes(color = Type)) +
#   geom_jitter(aes(color = Type), width = 0.05, alpha = 0.9) +
#   stat_boxplot(geom = 'errorbar', width = 0.25) +
#   ggtitle("Miles by Tractor Type") +
#   theme(legend.position = "bottom") +
#   geom_hline(yintercept = up) +
#   geom_hline(yintercept = low)
#   
# covp



grid.arrange(dp, sp, bp, densp)

