library(grid)
library(gridExtra)
library(ggplot2)

################## begin default settings #####################
data_dir <- "~/CSC458_Project/data/"
img_dir <- "~/CSC458_Project/r_plots/"

y_label <- "cummulate probability"
################## end default settings #####################

################## begin helper functions #####################
set_filePath<- function(dir, filename){
  return(paste(dir,filename, sep=""))
}

bool_switch <- function(num){
  if(num == 0){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

# return a table format
make_table <- function(data, title){
  table <- tableGrob(data)
  h <- grobHeight(table)
  w <- grobWidth(table)
  title <- textGrob(title, y=unit(0.5,"npc") + h, vjust=0, gp=gpar(fontsize=16))
  return(gTree(children=gList(table, title)))
}

cdf <- function(data, title, x_label){
  data <- data[!is.na(data)]
  
  precent_tb <- table(data)/length(data)
  precenatage <- cumsum(rep(unname(precent_tb)))
  packet_length <- (as.integer(names(precent_tb)))
  
  min_packlen <- min(packet_length)
  max_packlen <- max(packet_length)
  
  plot(packet_length, precenatage, main=title, xlab=x_label, ylab=y_label, col="red", type="p", xlim=c(min_packlen, max_packlen), ylim=c(0,1))
  lines(packet_length, precenatage, col="black")
  abline(h = 0, lty = 2)
  abline(h = 1, lty = 2)
}
################## begin helper functions #####################

# flows statistics
flows_statistics <- function(dir, tcp_flows, udp_flows){
  tcp_count <- length(tcp_flows$Packets)
  udp_count <- length(udp_flows$Packets)

  total_count <- tcp_count + udp_count
  
  tcp_stat <- data.frame(Protocol="TCP", Precentage=round(100*tcp_count/total_count, 2), FlowCount=tcp_count, TotalPackets=sum(tcp_flows$Packets))
  udp_stat <- data.frame(Protocol="UDP", Precentage=round(100*udp_count/total_count, 2), FlowCount=udp_count, TotalPackets=sum(udp_flows$Packets))
  
  df <- rbind(tcp_stat, udp_stat)
  
  png(set_filePath(dir, "flows_statistics.png"), height=100, width=400)
  grid.arrange(
    make_table(df, "Flows Statistics")
  )
  dev.off()
}

duration_cdfs <- function(dir ,tcp_flows, udp_flows){
  x_label <- "flows duration(ms)"
  
  tcp_duration <- tcp_flows$Duration
  udp_duration <- udp_flows$Duration
  total_duration <- c(tcp_duration, udp_duration)
  
  png(set_filePath(dir, "durations_cdfs.png"), height=800, width=1000)
  
  par(mfrow=c(2,2))
  cdf(total_duration, "Total flows duration cdf", x_label)
  cdf(tcp_duration, "TCP flows duration cdf", x_label)
  cdf(udp_duration, "UDP flows duration cdf", x_label)
  dev.off()
}

bytes_cdfs <- function(dir ,tcp_flows, udp_flows){
  x_label <- "flows bytes"
  
  tcp_bytes <- tcp_flows$Bytes
  udp_bytes <- udp_flows$Bytes
  total_bytes <- c(tcp_bytes, udp_bytes)
  
  png(set_filePath(dir, "bytes_cdfs.png"), height=800, width=1000)
  
  par(mfrow=c(2,2))
  cdf(total_bytes, "Total flows bytes cdf", x_label)
  cdf(tcp_bytes, "TCP flows bytes cdf", x_label)
  cdf(udp_bytes, "UDP flows bytes cdf", x_label)
  dev.off()
}

count_cdfs <- function(dir ,tcp_flows, udp_flows){
  x_label <- "flows packets count"
  
  tcp_count <- tcp_flows$Packets
  udp_count <- udp_flows$Packets
  total_count <- c(tcp_count, udp_count)
  
  png(set_filePath(dir, "count_cdfs.png"), height=800, width=1000)
  
  par(mfrow=c(2,2))
  cdf(total_count, "Total flows packets count cdf", x_label)
  cdf(tcp_count, "TCP flows packets count cdf", x_label)
  cdf(udp_count, "UDP flows packets count cdf", x_label)
  dev.off()
}
##################### main #####################

# loading data
tcp_flows <- read.csv(set_filePath(data_dir, "TCP_flows.csv"), header = TRUE)
udp_flows <- read.csv(set_filePath(data_dir, "UDP_flows.csv"), header = TRUE)

# plotting graphs
flows_statistics(img_dir ,tcp_flows, udp_flows)
duration_cdfs(img_dir ,tcp_flows, udp_flows)
bytes_cdfs(img_dir ,tcp_flows, udp_flows)
count_cdfs(img_dir ,tcp_flows, udp_flows)



