my.theme <- theme(axis.ticks.x = element_line(size = 0),
                  axis.ticks.y = element_line(size = 0.15),
                  axis.text = element_text(color = "black"),
                  axis.text.x = element_text(angle = 45, size = 12, hjust = 1, margin = margin(-7,20,-8,0, "pt")),
                  axis.title.x = element_blank(),
                  axis.title.y = element_text(size = 14),
                  plot.title = element_text(size = 18),
                  panel.background = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(colour = "red", linetype = "dotted",size = 0.25),
                  panel.grid.minor.y = element_line(colour = "red", linetype = "dotted", size = 0.25),
                  plot.margin = margin(c(10,10,10,10, "pt"))
)


chart.data.ages <- data.frame(goal = sapply(levels(sample.01.10$age), function(x) sample.01.10[sample.01.10$age == x,]$goal %>% sum), 
                         current = sapply(levels(sample.01.10$age), function(x) sample.01.10[sample.01.10$age == x,]$current %>% sum), 
                         age = as.factor(names(sapply(levels(sample.01.10$age), function(x) sample.01.10[sample.01.10$age == x,]$goal %>% sum))))
chart.data.ages$share.done <- chart.data.ages$current / chart.data.ages$goal
levels(chart.data.ages$age) <- levels(chart.data.ages$age) %>% rev

#text size adjusted for the whole final chart size
png(filename = "~/atlas-moskovsky-district-data/plots/sample-ages.png", width = 1200, height = 800)
ggplot(data = chart.data.ages) + 
  geom_bar(aes(age, goal), stat = "identity", fill = "#B2DF8A") +
  geom_bar(aes(age, current), stat = "identity", fill = "#1C52A8") +
  geom_text(aes(label = (round(chart.data.ages$share.done, digits = 2)*100) %>% paste("%", sep = ""), 
                x = age,
                y = goal,
                size = 16,
                hjust = -0.2
                )) +
  coord_flip() +
  my.theme + 
  theme(
    axis.ticks.x = element_line(size = 0),
    axis.ticks.y = element_line(size = 0),
    panel.grid.major.x = element_line(colour = "red", linetype = "dotted",size = 0.25),
    panel.grid.minor.x = element_line(colour = "red", linetype = "dotted",size = 0.25),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 12)
  ) + 
  xlab("Возраст") + 
  ylab("Количество анкет") +
  labs(title = "Прогресс сбора анкет по выборке по возрастам")
dev.off()
#--------------
levels(sample.01.10$district) <- c("Гагаринское", "Московская Застава", "Новоизмайловское", "Пулковский Меридиан", "Звёздное")
chart.data.districts <- data.frame(goal = sapply(levels(sample.01.10$district), function(x) sample.01.10[sample.01.10$district == x,]$goal %>% sum), 
                              current = sapply(levels(sample.01.10$district), function(x) sample.01.10[sample.01.10$district == x,]$current %>% sum), 
                              district = as.factor(names(sapply(levels(sample.01.10$district), function(x) sample.01.10[sample.01.10$district == x,]$current %>% sum)))
                              )
chart.data.districts$share.done <- chart.data.districts$current / chart.data.districts$goal

#---------------
png(filename = "~/atlas-moskovsky-district-data/plots/sample-districts.png", width = 1200, height = 800)
ggplot(data = chart.data.districts[order(chart.data.districts$share.done, decreasing = T),]) + 
  geom_bar(aes(reorder(district, -share.done), goal), stat = "identity", fill = "#B2DF8A") +
  geom_bar(aes(reorder(district, -share.done), current), stat = "identity", fill = "#1C52A8") +
  geom_text(aes(label = (round(chart.data.districts[order(chart.data.districts$share.done, decreasing = T),]$share.done, digits = 2)*100) %>% paste("%", sep = ""), 
                x = reorder(district, -share.done),
                y = goal,
                size = 16,
                hjust = -0.2
  )) +
  coord_flip() +
  my.theme + 
  theme(
    axis.ticks.x = element_line(size = 0),
    axis.ticks.y = element_line(size = 0),
    panel.grid.major.x = element_line(colour = "red", linetype = "dotted",size = 0.25),
    panel.grid.minor.x = element_line(colour = "red", linetype = "dotted",size = 0.25),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text(size = 10, angle = 30,  margin = margin(0, -20, 0, 0, "pt")),
    axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 12)
  ) + 
  xlab("Муниципальный округ") + 
  ylab("Количество анкет") +
  labs(title = "Прогресс сбора анкет по выборке по муниципальным округам")
dev.off()
