library(shiny)
library(shinyjs)
library(shinythemes)
library(igraph)
library(vcd)
library(textdata)
library(MASS)
library(RColorBrewer)
library(datasets)
library(corrgram)
library(visdat)
library(forecast)
library(tidytext)
library(tidyverse)
library(janeaustenr)
library(stringr)
library(wordcloud2)
library(reshape2)
library(pls)
library(ggplot2)
library(plotly)
library(visNetwork)
library(leaflet)
library(shinycssloaders)
library(car)
#library(tabplot)
library("readxl")
library(forecast)
library(lubridate)
library(ggplot2)
library(reshape2)
library(magrittr)
library(readxl)
library(data.table)
library(lubridate)
library(stringr)
library(tidyr)
library(dplyr)
library(RColorBrewer)


max_plots <- 5
comp.years="2007-2008"

dat01 <- read.csv("DATA_19960716_20201101_daily.csv", header = TRUE)

dat01$date = as.Date(dat01$date, format = "%Y-%m-%d")

dat01 = dat01[dat01$station != "Webb Raws", ]

dat01 = reshape2::dcast(data = dat01, formula = station + date ~ product, value.var = "value")

colnames(dat01)[colnames(dat01)=="station"] <- "STN"
colnames(dat01)[colnames(dat01)=="date"] <- "DATE"
colnames(dat01)[colnames(dat01)=="Observed Daily Build Up Index"] <- "BUI"
colnames(dat01)[colnames(dat01)=="Observed Daily Drought Code"] <- "DC"
colnames(dat01)[colnames(dat01)=="Observed Daily Fire Severity Rating"] <- "CDSR"

dat02 = dat01 %>%
  group_by(STN) %>%
  complete(DATE = seq.Date(min(DATE), max(DATE), by="day")) 


# Create SEASON.START column, which shows when the fire season started for each observation.
#   Also create MONTH and MONTH.DAY columns:

dat03 = transform(
  dat02,
  SEASON.START=ifelse(substr(DATE,6,10)>="07-01",
                      paste(substr(DATE,1,4), "07-01",sep="-"),
                      ifelse(substr(DATE,6,10)<="06-30",
                             paste(as.numeric(substr(DATE,1,4))-1, "07-01",sep="-"),
                             "xx")),
  MONTH=as.numeric(substr(DATE,6,7)),
  MONTH.DAY=substr(DATE,6,10))


# Remove 29Feb not to mess up the trends in the graphs (throws off historical averages etc.)
dat04 = dat03[dat03$MONTH.DAY != "02-29", ]


begin.yr = "0001"
end.yr = "0002"

# Assign appropriate beginning and end years to the MONTH.DAY column depending on
#     whether the given observation is before or after fire season start
#   Also, create SEASON column:
dat05 = transform(dat04, MONTH.DAY=as.Date(paste(ifelse(MONTH>=7, begin.yr, ifelse(MONTH<=6, end.yr, "xx")),
                                                 MONTH.DAY, sep="-")),
                  SEASON=paste(substr(SEASON.START,1,4),as.numeric(substr(SEASON.START,1,4))+1,sep="-"))


# Check if any MONTH.DAY values are NA:

dat05[is.na(dat05$MONTH.DAY), ]
# *** NAs will appear in the above output if the end of the current fire season is not a leap year, and that's ok
#     (since we don't have February 29 in the current fire season, it's ok not to graph it)


# We don't need the SEASON.START column now that the SEASON column is created, so delete SEASON.START:
dat06=within(dat05, rm(SEASON.START))  


# Figuring out if the fire season is full or partial (because CDSR is meaningless for partial seasons):
dat07 = mutate(group_by(dat06, STN, SEASON),
               SCOPE = ifelse(min(MONTH.DAY, na.rm = T) > as.Date(paste0(begin.yr, "-", "09-01")), "partial", "full"))


# Read in a list of all stations and corresponding regions:
stninfo = read.csv("FWSYS_station_info.csv")
regions = stninfo[, c("ISLAND", "DIVISION", "REGION", "SUBREGION", "STN", "STATUS")]


# merge dat01 dataframe with the regions dataframe to assign a region to each row based on station:
dat08 = merge(dat07, regions, all.x = T)


# Sort dat02 by ISLAND, then REGION, then SUBREGION, then STN, then DATE:
dat09 <- dat08[order(dat08$ISLAND, dat08$REGION, dat08$SUBREGION, dat08$STN, dat08$DATE),] 


# If REGION is NA, make it into "unknown region":
dat09$REGION = ifelse(is.na(dat09$REGION), "unknown region", as.character(dat09$REGION))


# If ISLAND is NA, make it into "unknown island":
dat09$ISLAND = ifelse(is.na(dat09$ISLAND), "unknown island", as.character(dat09$ISLAND))

dat09[is.na(dat09$CDSR), ]$CDSR = 0

# calculate cumulative sum; data.table() command requires "data.table" package
dat10 <- as.data.frame(data.table(dat09)[, CDSR := cumsum(CDSR), by=list(SEASON, STN)])


# If SCOPE is partial, make CDSR into NA (since cumulative CDSR is meaningless for partial seasons)
dat11 = transform(dat10, CDSR = ifelse(SCOPE == "partial", NA, CDSR))

dat11 = within(dat11, rm(SCOPE)) 



dat1 = dat11

outlook_theme = theme(panel.background=element_rect(colour=NA,fill = "white"),
                      axis.line = element_line(colour = "black"),
                      axis.text = element_text(size = 16),
                      axis.title=element_text(size = 16),
                      axis.title.x = element_text(margin = margin(t = 7)),
                      axis.title.y = element_text(margin = margin(r = 10)),
                      plot.title = element_text(size=18, margin = margin(t = 3, b = 20)),
                      legend.text=element_text(size = 14),
                      legend.title=element_text(size = 14),
                      text = element_text(size = 18),
                      legend.key = element_rect(fill = "white"),
                      legend.background = element_rect(fill = NA),
                      legend.position = "right",
                      panel.grid.major.y = element_line(colour = "grey", size = 0.3),
                      panel.grid.major.x = element_blank(),
                      panel.grid.minor.x = element_blank(),
                      panel.grid.minor.y = element_blank(),
                      axis.ticks.y = element_blank())

n <- 25
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
my_pal = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

my_pal = c("black", "green4", "darkorchid2", "orange", "cyan", "magenta", my_pal)


regions = unique(dat1$REGION)

plot_station = function(data, station, variable, comp_years,
                        min_year = NULL, max_year = NULL,
                        x_lim = NULL, y_lim = NULL){
  
  stn_data = data[data$STN == station, ]
  
  last_season = as.character(stn_data[stn_data$DATE == max(stn_data$DATE), ]$SEASON[1])
  
  
  if(comp_years == "all"){
    comp_years = as.character(unique(stn_data[stn_data$SEASON != last_season, ]$SEASON))
  }
  
  if(is.null(min_year)){
    min_year = as.numeric(substr(min(stn_data[stn_data$STN == station, ]$DATE), 1, 4))
  } else {
    stn_data = stn_data[substr(stn_data$SEASON, 1, 4) >= min_year, ]
  }
  
  if(is.null(max_year)){
    max_year = as.numeric(substr(max(stn_data[stn_data$STN == station, ]$DATE), 1, 4))
  } else {
    stn_data = stn_data[substr(stn_data$SEASON, 6, 9) <= max_year, ]
  }
  
  if(variable == "BUI"){
    variable_label = "Buildup Index"
  } else if(variable == "CDSR"){
    variable_label = "Cumulative Daily Severity Rating"
  } else if(variable == "DC"){
    variable_label = "Drought Code"
  } else {
    variable_label = variable
  }
  
  
  record_length = max_year - min_year
  
  range_label = paste0('range: ', record_length, " yrs\n(", min_year, " to ", max_year, ")")
  
  
  plot_title = paste0(station,
                      ifelse(any(stn_data[!is.na(stn_data$STATUS), ]$STATUS == "discontinued"),
                             " - DISCONTINUED",
                             ""))
  
  last_date = max(stn_data[!is.na(stn_data$BUI), ]$DATE, na.rm = T)
  last_value = stn_data[stn_data$DATE == last_date, variable][1]
  
  current_label = paste0("\nCurrent ", variable, ":\n",
                         round(last_value, 1), "\n(", last_date, ")")
  
  
  comp_years_real = comp_years[comp_years %in% stn_data$SEASON]
  
  if(length(comp_years_real) > 0){
    line_colors = c(my_pal[1:length(comp_years_real)], "blue", "red")
  } else {
    line_colors = c("blue", "red")
  }
  
  
  legend_variables = c("average", comp_years_real, last_season)
  
  
  p1 = ggplot(data = stn_data[stn_data$SEASON != last_season, ],
              aes_string(x = "MONTH.DAY", y = variable)) +
    stat_summary(geom = "ribbon", fun.ymin = "min", fun.ymax = "max",
                 aes(fill = range_label), alpha = 0.25) +
    stat_summary(geom = "line", fun.y = "mean", aes(color = "average", size = "average")) +
    geom_line(data = stn_data[stn_data$SEASON %in% c(comp_years_real, last_season), ],
              aes(color = SEASON, size = SEASON)) +
    geom_ribbon(aes(ymin=0, ymax=0, fill="white"), lwd=0.2, alpha=0.25) + # blank ribbon for extra label+
    scale_fill_manual(values = c("black", NA), 
                      name = '',guide = 'legend',
                      labels = c(range_label, current_label)) +
    scale_color_manual(values = line_colors, name = "", breaks = legend_variables) +
    scale_size_manual(values = c(rep(0.2, length(comp_years_real)), 1, 1), name = "",
                      breaks = legend_variables) +
    scale_x_date(date_labels = "%b",date_minor_breaks = "15 day", date_breaks = "1 month") +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    guides(colour = guide_legend(order = 1),
           size = guide_legend(order = 1),
           fill = guide_legend(order = 2, label.vjust = 0.7)) +
    labs(title = plot_title,
         x = "Date",
         y = variable_label) +
    outlook_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'right',
          legend.text=element_text(size=10), legend.key.height = unit(1,"line"),
          legend.margin=margin(t=0, r=0, b=-0.6, l=-0.3, unit="cm"),
          legend.title = element_blank())
  
  return(p1)
}


regions = unique(dat1$REGION)

largelist.bui = list()


set.seed(15)
cname <- letters[1:3]
data <- data.frame(
  vals = rpois(10*length(cname),15),
  customer = rep(cname,each=10)
)
# # 
# # # # reg = 1
# # # # reg = match("Mid-South Canterbury", regs)
# # # # i = 17
# # #
# # #
# # # # data = dat1
# # # # region = "Mid-South Canterbury"
# # # # variable = "BUI"
# # #



# plot_save_region = function(data, region, variable) {
# 
#   mydat = data[data$REGION == region, ]
#   stns = sort(unique(mydat$STN))
# 
#   max_y = ifelse(!all(is.na(mydat[, variable])), max(mydat[, variable], na.rm = T), 0.1)
#   y_limit = c(0, max_y)
# 
#   for (i in seq_along(stns)) {
# 
#     station_plot = plot_station(dat1, station = stns[i], variable = variable,
#                                 comp_years = comp.year, y_lim = y_limit)
#     return(station_plot)
#   }
#   graphics.off()
# 
# }

# my4 <-  input$newcomp
# my5<-input$newcomp+1
# my6 <- str_c(c(my4,my5), collapse = "-")
# #plot_save_region(data = dat1, region = input$region, variable = input$indexxx)
# mydat = dat1[dat1$REGION == region, ]
# stns = sort(unique(mydat$STN))
# max_y = ifelse(!all(is.na(mydat[, input$indexxx])), max(mydat[, input$indexxx], na.rm = T), 0.1)
# y_limit = c(0, max_y)

