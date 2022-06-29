rm(list = ls())
source("init.r", encoding = "UTF-8")

aq_data = loadData("prepared.rData")

ggplot(aq_data[basetime >= "2016-01-01" & variable %in% c("35d mean", "365d mean")], 
       aes(x = basetime, y = value, color = variable)) +
  #geom_point(size = 0.1) +
  geom_line() +
  #geom_line(aes(y = variable), color = "blue", size = 0.2, linetype = "dotted") +
  #geom_line(aes(y = no2_rmean35), color = "red") +
  #geom_line(aes(y = no2_rmean365), color = "black") +
  #geom_text(data = NULL, aes(x = as.Date("2017-01-01"), y = 1), label = "adsf") +
  facet_wrap(~name_adv) +
  scale_x_date(labels = date_format("%Y"), date_breaks = "1 year")  + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  ylab("NO2 surface concentration [µg/m³]") +
  xlab("Date")

ggsave("plots/plot.png", width = 20, height = 12)

for(n in unique(aq_data$name)){
  #n = "Luxembourg"
  
  ggplot(aq_data[basetime >= "2016-01-01" & variable %in% c("7d mean", "365d mean") & name == n], 
         aes(x = basetime, y = value, color = variable, linetype = variable)) +
    #geom_point(size = 0.1) +
    geom_line() +
    #geom_line(aes(y = variable), color = "blue", size = 0.2, linetype = "dotted") +
    #geom_line(aes(y = no2_rmean35), color = "red") +
    #geom_line(aes(y = no2_rmean365), color = "black") +
    #geom_text(data = NULL, aes(x = as.Date("2017-01-01"), y = 1), label = "adsf") +
    facet_wrap(~name) +
    geom_vline(aes(xintercept = as.Date("2020-03-01")), linetype = "dashed", size = 0.2) +
    geom_text(aes(x = as.Date("2020-03-01"), 
                  y = mean(value)*1.9, label = "Mar 2020"), angle=90,
              size = 3, color = "black", vjust = -0.5, hjust="outward") +
    scale_x_date(labels = date_format("%b %Y"), date_breaks = "1 month")  + 
    theme_light() + 
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "bottom",
          legend.title = element_blank()) +
    ylab("NO2 surface concentration [µg/m³]") +
    xlab("Date") +
    scale_linetype_manual(values=c("dotted", "solid"))
  
  ggsave(paste0("plots/plot_", n, ".png"), width = 7, height = 5)
  
}

ggplot(aq_data[basetime >= "2016-01-01" & variable %in% c("365d mean") & 
                 name %in% c("Amsterdam", "Vienna")], 
       aes(x = basetime, y = value, color = name, linetype = variable)) +
  #geom_point(size = 0.1) +
  geom_line() +
  #geom_line(aes(y = variable), color = "blue", size = 0.2, linetype = "dotted") +
  #geom_line(aes(y = no2_rmean35), color = "red") +
  #geom_line(aes(y = no2_rmean365), color = "black") +
  #geom_text(data = NULL, aes(x = as.Date("2017-01-01"), y = 1), label = "adsf") +
  #facet_wrap(~name) +
  geom_vline(aes(xintercept = as.Date("2020-03-01")), linetype = "dashed", size = 0.2) +
  geom_text(aes(x = as.Date("2020-03-01"), 
                y = mean(value), label = "Mar 2020"), 
            angle=90,
            fontface = "plain",
            size = 2.5, color = "black", vjust = -0.5, hjust="outward") +
  scale_x_date(labels = date_format("%b %Y"), date_breaks = "1 month", 
               expand = c(0, 0))  + 
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top",
        legend.title = element_blank()) +
  ylab("surface concentration [µg/m³]") +
  xlab("Date") +
  labs(title = "CAMS NO2 daily mean analysis Amsterdam vs. Vienna", 
       #subtitle = "365 day mean",
       caption = "@decarbnow; data from @CopernicusECMWV")

ggsave(paste0("plots/plot_Amsterdam_vs_Vienna.png"), width = 7, height = 4)