library(tidyverse)

TikTok_data <- dataset_tiktok_scraper_1_ %>%
  filter(!is.na(`Final Categories Sarah`))
plot0 <- TikTok_data %>%
  ggplot() +
  geom_boxplot(aes(y=diggCount/1000000)) +
  theme_bw() +
  ylab("Plays (in 1.000.000)") +
  theme(axis.text.x = element_blank())+
  xlab("All 100 videos")


plot1 <- TikTok_data %>%
  ggplot() +
  geom_boxplot(aes(y=diggCount/100000)) +
  theme_bw() +
  ylab("Likes (in 100.000)") +
  theme(axis.text.x = element_blank())+
  xlab("All 100 videos")

plot2 <- TikTok_data %>%
  ggplot() +
  geom_boxplot(aes(y=shareCount/10000)) +
  theme_bw() +
  ylab("Shares (in 10.000)") +
  theme(axis.text.x = element_blank())+
  xlab("All 100 videos")

plot3 <- TikTok_data %>%
  ggplot() +
  geom_boxplot(aes(y=commentCount/1000)) +
  theme_bw() +
  ylab("Comments (in 1.000)") +
  theme(axis.text.x = element_blank())+
  xlab("All 100 videos")

library(ggpubr)
combined_plot <- ggarrange(plot0, plot1, plot2, plot3, ncol = 4, nrow = 1)  

ggsave(filename="combined_boxplots.png", combined_plot, width = 10, height = 5)


names(table(TikTok_data$`Final Categories Sarah`))

critical <- c("AI detectors as threat","failures to calculate answers",
              "filters too restrictive", "GPTZero", "Server breakdowns" )
promotional <- c("answer questions" , "C#","cactus AI" , "Chatsonic",
                 "excel", "get research ideas", "prompt engineering" ,
                 "python", "quillbot",  "Quillbot",
                 "take meeting notes from recording", "Tutorly.AI",
                 "write code","write code (HTML, JavaScript, CSS)", 
                 "write essays", "write other texts")

TikTok_data <- TikTok_data %>%
  mutate(largeCat=ifelse(`Final Categories Sarah` %in% promotional, "1. Promotional",
                         ifelse(`Final Categories Sarah` %in% critical, "2. Critical",
                               "3. Others")))
  

get_box_stats <- function(y) {
  return(data.frame(
    y = 1,
    label = paste(
      "Count =", length(y), "\n",
      "Mean =", round(mean(y), 2), "\n",
      "Median =", round(median(y), 2), "\n"
    )
  ))
}

plot0_by_group <- TikTok_data %>%
  ggplot(aes(y=diggCount/1000000, x=as.factor(largeCat))) +
  geom_boxplot() +
  theme_bw() +
  ylab("Plays (in 1.000.000)") + xlab("")+
  stat_summary(fun.data = get_box_stats, geom = "text");
plot0_by_group
  
get_box_stats <- function(y) {
  return(data.frame(
    y = 10,
    label = paste(
      "Count =", length(y), "\n",
      "Mean =", round(mean(y), 2), "\n",
      "Median =", round(median(y), 2), "\n"
    )
  ))
}

plot1_by_group <- TikTok_data %>%
  ggplot(aes(y=diggCount/100000, x=as.factor(largeCat))) +
  geom_boxplot() +
  theme_bw() +
  ylab("Likes (in 100.000)") + xlab("")+
  stat_summary(fun.data = get_box_stats, geom = "text");

plot2_by_group <- TikTok_data %>%
  ggplot(aes(y=shareCount/10000, x=as.factor(largeCat))) +
  geom_boxplot() +
  theme_bw() +
  ylab("Shares (in 10.000)") + xlab("")+
  stat_summary(fun.data = get_box_stats, geom = "text");

plot3_by_group <- TikTok_data %>%
  ggplot(aes(y=commentCount/1000, x=as.factor(largeCat))) +
  geom_boxplot() +
  theme_bw() +
  ylab("Likes (in 1.000)") + xlab("")+
  stat_summary(fun.data = get_box_stats, geom = "text");


combined_plot_by_group <- ggarrange(plot0_by_group, plot1_by_group, plot2_by_group, plot3_by_group, ncol = 2, nrow = 2)  

ggsave(filename="combined_boxplots_by_group.png", combined_plot_by_group, width = 10, height = 10)
