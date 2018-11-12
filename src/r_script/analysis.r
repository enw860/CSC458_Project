# install inbrary if necessary
# install.packages("grid")
# install.packages("gridExtra")

library(grid)
library(gridExtra)

# default setting (set your own absoulte path)
# data file version: https://drive.google.com/file/d/1fqSrRpoIXx8eFPOvyMus-nMprTK6HdYI/view?usp=drive_web 
root_dir <- "/Users/lionel/Desktop/CSC458/Assignment/project_due_1125/CSC458_Project/r_plots/"

set_filePath<- function(filename){
  return(paste(root_dir,filename, sep=""))
}

# loading data(might take a while)
dat <- read.csv(set_filePath("package_dissection_incsv"), header = TRUE)

# plots for per-pack analysis bullet point 1
plot_protocal_precent_table <- function(precent_data, filepath){
  # initial settings
  title <- "Protocal Precent Table"
  mydf <- data.frame(precent_data)
  
  # plot the precentage table
  png(filepath, height=550, width=500)
  grid.arrange(
    top=title,
    tableGrob(mydf[1:12, 1:2]),
    tableGrob(mydf[13:24, 1:2]),
    tableGrob(mydf[25:35, 1:2]),
    tableGrob(mydf[36:46, 1:2]),
    nrow=2)
  dev.off()
}

# plots for per-pack analysis bullet point 1
plot_global_precent_pie <- function(precent_data, filepath, aggregate_std=1){
  # initial settings
  aggregate_point <- 0
  n <- length(precent_data$Protocal_Type)
  types <- precent_data$Protocal_Type
  precent <- precent_data$Precentage
  
  # find the aggregrate point index
  for(i in 1:n){
    if(precent[i] < aggregate_std){
      aggregate_point = i-1
      break
    }
  }
  
  # aggregrate data
  types <- c(types[1:aggregate_point], "Other")
  precent <- c(precent[1:aggregate_point], 100-sum(precent[1:aggregate_point]))

  # plot pie chart
  png(filepath, height=400, width=500)
  par(mfrow=c(1,1))
  pie(precent, labels=paste(round(precent,1), "%-", types, sep = ""), 
      main="Protocal Precent Pie", radius = 1)

  dev.off()
}

# still woking on that
# plots for per-pack analysis bullet point 2
plot_totlal_packlen_cdf <- function(filepath){
  png(filepath, height=400, width=500)
  
  par(mfrow=c(2,2))
  plot(ecdf(dat$Length))
  plot(ecdf(dat$Length[dat$Protocol == "TCP"]))
  plot(ecdf(dat$Length[dat$Protocol == "UDP"]))
  plot(ecdf(dat$Length[dat$Protocol == "IPv4"]))
  
  dev.off()
}

# still woking on that
# plots for per-flow analysis bullet point 4
plot_totlal_packlen_cdf <- function(filepath){
  png(filepath, height=400, width=500)
  
  par(mfrow=c(2,2))
  plot(ecdf(dat$Time))
  plot(ecdf(dat$Time[dat$Protocol == "TCP"]))
  plot(ecdf(dat$Time[dat$Protocol == "UDP"]))
  plot(ecdf(dat$Time[dat$Protocol == "IPv4"]))
  
  dev.off()
}

################## main ############################

# load data
len <- length(dat$Protocol)
precent_tb <- sort(round((table(dat$Protocol)/len)*100, 4), decreasing=TRUE)

# process data
types <- names(precent_tb)
precenatage <- rep(unname(precent_tb))
precent_data <- list("Protocal_Type"=types, "Precentage"=precenatage)

# plot graphs
plot_protocal_precent_table(precent_data, set_filePath("Proto_tbl.png"))
plot_global_precent_pie(precent_data, set_filePath("Proto_pie.png"), 2)

# still woring on these two functions
# plot_totlal_packlen_cdf(set_filePath("Length_cdf_comp.png"))
# plot_totlal_packlen_cdf(set_filePath("Arr_time_cdf_comp.png"))