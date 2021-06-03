
library(CBRT)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(plotly)
library(Rmisc)

searchCBRT(c("GDP","balance"))

showSeriesNames("bie_gsyihhy")

myCBRTKey <- "HAtzh5JZmg"

#Obtain Quarterly GDP Data. "GDP calculated with fixed prices" is chosen.

GDP_Ex_Fix_1998_TRY_Thousand <- getDataSeries("TP.UR.GG01.S")

#To Get Year Over Year GDP Growth Data

GDP_Ex_Fix_1998_TRY_Thousand[, GDP_Growth := 100*(TP.UR.GG01.S - shift(TP.UR.GG01.S, n = 4L))/shift(TP.UR.GG01.S, n = 4L)]

GDP_Ex_Fix_1998_TRY_Thousand[, TP.UR.GG01.S := NULL]

#Deleting Missing Values

GDP_Growth_1 <- na.omit(GDP_Ex_Fix_1998_TRY_Thousand)

#

showSeriesNames("bie_odana6")

#Obtain Current Account Data. Frequency is chosen as "6", which means "Quarterly".

CAD_Mil_USD <- getDataSeries("TP.ODANA6.Q01", freq = 6)

#To make Growth and CAD's time range identical, some rows are deleted from CAD data.

CAD_Mil_USD_1 <- CAD_Mil_USD[-c(1:196, 267:285),]

#Year variable is created. Its values will be in the form "Year-Qi".

aa <- paste0(rep(seq(1999,2016), each = 4, length = 70), rep("-"), rep(c("Q1","Q2","Q3","Q4")))

GDP_Growth_1[, Year := aa]

CAD_Mil_USD_1[, Year := aa]

#Deleting time variable which is unnecessary.

GDP_Growth_1[, time := NULL]

CAD_Mil_USD_1[, time := NULL]

#Changing the Current Account variable's name as "CAD".

names(CAD_Mil_USD_1)[names(CAD_Mil_USD_1) == "TP.ODANA6.Q01"] <- "CAD"

#Change the order of columns to make the "Year" column first.

setcolorder(CAD_Mil_USD_1, c("Year", "CAD"))

setcolorder(GDP_Growth_1, c("Year", "GDP_Growth"))

#To make Growth and CAD's numeric values close to each other, CAD values will be transformed to "Billion USD" from "Million USD".

CAD_Bil_USD <- CAD_Mil_USD_1[, CAD := CAD/1000]

#To put both CAD and Growth into the same plot, following data table is created.

DT1 <- GDP_Growth_1[CAD_Bil_USD, on = "Year"]

#Function of the theme which will be used on the plots.

theme_omer <- function() { 
  theme(plot.title = element_text(color = "#3182bd",size = 24, face = "bold"),
        plot.subtitle = element_text(color = "#2b8cbe", size = 16, face = "bold"),
        panel.grid.major.x = element_line(size = 0.2, colour = "#a6cee3", linetype = "solid"),
        panel.grid.minor.x = element_line(size = 0.1, colour = "#a6cee3"),
        panel.grid.major.y = element_line(size = 0.2, colour = "#a6cee3", linetype = "solid"),
        panel.grid.minor.y = element_line(size = 0.1, colour ="#a6cee3"),
        panel.background = element_rect(fill = "#f7f7f7"),
        axis.line = element_line(size = 1.4, colour = "#9ecae1"),
        axis.title = element_text(size = 15, colour = "gray26", face = "bold"),
        axis.text = element_text(size = 12, colour = "gray38",face = "bold"),
        axis.ticks.x = element_line(size = 0.5),
        axis.ticks.y = element_line(size = 0.5),
        axis.ticks.length.y = unit(0.4,"cm"),
        axis.ticks.length.x = unit(0.4,"cm"),
        axis.title.x = element_text(vjust = 1),
        axis.text.x = element_text(angle = 45),
        axis.title.y = element_text(vjust = 2),
        plot.caption = element_text(size = 10,color = "#636363"),
        legend.position = "right",
        legend.title = element_text(size = 12)
  )
}

#To prevent x axis text's from overlapping to each other, some x axis texts is deleted by creating the vector "b" and adding "scale_x_discrete(breaks = b)" to plots. Also, angle of the texts is changed above in the theme function.  

b <- paste0(rep(seq(1999,2016), each = 1, length = 18), rep("-"), rep("Q1"))

#Horizontal line "Y = 0" is placed in the plots to make the origin more visible. "group = 1" is used to prevent an error. "Colorblind safe" colors are used in the plots.

#Plot of CAD

g1 <- ggplot(data = CAD_Bil_USD, aes(y = CAD, x = Year, group = 1)) + 
  geom_line(size = 1.5, color = "#ef6548") + theme_omer() + scale_x_discrete(breaks = b) + 
  geom_hline(yintercept = 0, color ="#2171b5", size = 1) +
  labs(title = "Current Account",
       subtitle = "Turkey",
       x = "Year", y = "CAD(Billion USD)",
       caption = "Source: CBRT")

ggplotly(g1) %>%
  layout(annotations = 
           list(x = 1, y = -0.3, text = "Source: CBRT", 
                showarrow = F, xref = 'paper', yref = 'paper', 
                xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0,
                font = list(size = 12, color = "#636363")), 
         title = list(text = paste0('Current Account',
                                    '<br>',
                                    '<sup>',
                                    'Turkey',
                                    '</sup>'))) %>% 
  add_annotations(x = 0, y = -0.3, text = "Figure 1", 
                  showarrow = F, xref = 'paper', yref = 'paper', 
                  xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0, 
                  font = list(size = 15, color = "gray26", face = "bold"))

#Plot of GDP Growth

g2 <- ggplot(data = GDP_Growth_1, aes(y = GDP_Growth, x = Year, group = 1)) + 
  geom_line(size = 1.5, color = "#ef6548") + theme_omer() + scale_x_discrete(breaks = b) +
  geom_hline(yintercept = 0, color ="#2171b5", size = 1) +
  labs(title = "GDP Growth",
       subtitle = "Turkey",
       x = "Year", y = "Growth(%)",
       caption = "Source: CBRT")

ggplotly(g2) %>%
  layout(annotations = 
           list(x = 1, y = -0.3, text = "Source: CBRT", 
                showarrow = F, xref = 'paper', yref = 'paper', 
                xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0,
                font = list(size = 12, color = "#636363")), 
         title = list(text = paste0('GDP Growth',
                                    '<br>',
                                    '<sup>',
                                    'Turkey',
                                    '</sup>'))) %>% 
  add_annotations(x = 0, y = -0.3, text = "Figure 2", 
                  showarrow = F, xref = 'paper', yref = 'paper', 
                  xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0, 
                  font = list(size = 15, color = "gray26", face = "bold"))

#Plot of CAD & GDP Growth

g3 <- ggplot(data = DT1, aes(x = Year, group = 1)) + 
  geom_line(aes(y = GDP_Growth, color = "GDP Growth(%)"), size = 1.5) +
  geom_line(aes(y = CAD, color = "CAD(Billion Dollar)"), size = 1.5) + theme_omer() + 
  scale_x_discrete(breaks = b) + geom_hline(yintercept = 0, color ="#2171b5", size = 1) +
  guides(col = guide_legend("")) +
  labs(title = "GDP Growth & Current Account",
       subtitle = "Turkey",
       x = "Year", y = "Growth and CAD",
       caption = "Source: CBRT")

ggplotly(g3) %>%
  layout(legend = list(x = 0.01, y = 0.01, bgcolor = "#E2E2E2"), 
         annotations = list(x = 1, y = -0.3, text = "Source: CBRT", 
                            showarrow = F, xref = 'paper', yref = 'paper', 
                            xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0,
                            font = list(size = 12, color = "#636363")), 
         title = list(text = paste0('GDP Growth & Current Account',
                                    '<br>',
                                    '<sup>',
                                    'Turkey',
                                    '</sup>'))) %>% 
  add_annotations(x = 0, y = -0.3, text = "Figure 3", 
                  showarrow = F, xref = 'paper', yref = 'paper', 
                  xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0, 
                  font = list(size = 15, color = "gray26", face = "bold"))



p1 <- ggscatter(DT1, x = "CAD", y = "GDP_Growth", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "CAD(Bil.USD)", ylab = "Growth(%)") + labs(title = "1999-2016")

p2 <- ggscatter(DT1[c(13:40)], x = "CAD", y = "GDP_Growth", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "CAD(Bil.USD)", ylab = "Growth(%)") + labs(title = "2002-2008")

p3 <- ggscatter(DT1[c(41:52)], x = "CAD", y = "GDP_Growth", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "CAD(Bil.USD)", ylab = "Growth(%)") + labs(title = "2009-2011")

p4 <- ggscatter(DT1[c(53:70)], x = "CAD", y = "GDP_Growth", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "CAD(Bil.USD)", ylab = "Growth(%)") + labs(title = "2012-2016")

p5 <- ggscatter(DT1[c(9:20)], x = "CAD", y = "GDP_Growth", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "CAD(Bil.USD)", ylab = "Growth(%)") + labs(title = "2001-2003") +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE)


multiplot(p1, p2, p3, p4, p5, cols = 3)










