library(grid)
library(gridExtra)
library(ggplot2)

# default setting (set your own absoulte path)
data_dir <- "~/CSC458_Project/data/"
img_dir <- "~/CSC458_Project/r_plots/"

plot_scatter <- TRUE
plot_ecdf <- TRUE
plot_cdf <- TRUE

other_net_protocol <- c("MS NLB", "LLC", "IPX", "Intel ANS probe")
other_trans_protocol <- c("VRRP", "PIMv0", "OSPF", "NCS", "IGMPv0", "GRE", "ESP", "IGRP")

set_filePath<- function(dir, filename){
  return(paste(dir,filename, sep=""))
}

# plot precentage tables
plot_precentage_tables <- function(data, dir){
  trans_stat <- transport_statistics(data)
  net_stat <- network_statistics(data, trans_stat)
  link_stat <- link_statistics(data, net_stat)
  
  # plot tables
  png(set_filePath(dir, "packets_precentage_statistics.png"), height=350, width=400)
  grid.arrange(
    make_table(link_stat, "Link Layer Packets Statistics"),
    make_table(net_stat, "Network Layer Packets Statistics"),
    make_table(trans_stat, "Transport Layer Packets Statistics")
  )
  dev.off()
}

# count packets and size of packets in Transport Layer
transport_statistics <- function(data){
  tcp_filter <- data$frame.len[data$X_ws.col.Protocol=="TCP"]
  udp_filter <- data$frame.len[data$X_ws.col.Protocol=="UDP"]
  other_filter <- data$frame.len[data$X_ws.col.Protocol %in% other_trans_protocol]
  
  tcp_count <- length(tcp_filter)
  udp_count <- length(udp_filter)
  other_count <- length(other_filter)
  
  total_count <- tcp_count + udp_count + other_count
  
  tcp_stat <- data.frame(Protocol="TCP",Precentage=round(100*tcp_count/total_count, 2),PackageCount=tcp_count,TotalSize=sum(tcp_filter))
  udp_stat <- data.frame(Protocol="UDP",Precentage=round(100*udp_count/total_count, 2),PackageCount=udp_count,TotalSize=sum(udp_filter))
  other_stat <- data.frame(Protocol="Other",Precentage=round(100*other_count/total_count, 2),PackageCount=other_count,TotalSize=sum(other_filter))
  
  df <- rbind(tcp_stat, udp_stat)
  df <- rbind(df, other_stat)
  
  return(df)
}

# count packets and size of packets in Network Layer
network_statistics <- function(data, transLayer){
  ip_filter <- data$frame.len[data$X_ws.col.Protocol %in% c("IPv4", "IPv6")]
  icmp_filter <- data$frame.len[data$X_ws.col.Protocol %in% c("ICMP", "ICMPv6")]
  other_filter <- data$frame.len[data$X_ws.col.Protocol %in% other_net_protocol]
  
  ip_count <- length(ip_filter) + sum(transLayer$PackageCount)
  icmp_count <- length(icmp_filter)
  other_count <- length(other_filter)
  
  total_count <- ip_count + icmp_count + other_count
  
  ip_stat <- data.frame(Protocol="IP",Precentage=round(100*ip_count/total_count, 2),PackageCount=ip_count,TotalSize=sum(ip_filter)+sum(transLayer$TotalSize))
  icmp_stat <- data.frame(Protocol="ICMP",Precentage=round(100*icmp_count/total_count, 2),PackageCount=icmp_count,TotalSize=sum(icmp_filter))
  other_stat <- data.frame(Protocol="Other",Precentage=round(100*other_count/total_count, 2),PackageCount=other_count,TotalSize=sum(other_filter))
  
  df <- rbind(ip_stat, icmp_stat)
  df <- rbind(df, other_stat)
  
  return(df)
}

# count packets and size of packets in Link Layer
link_statistics <- function(data, netLayer){
  arp_filter <- data$frame.len[data$X_ws.col.Protocol == "ARP"]
  
  arp_count <- length(arp_filter) 
  E802q_count <- sum(netLayer$PackageCount)
  
  total_count <- arp_count + E802q_count
  
  E802q_stat <- data.frame(Protocol="Ethernet",Precentage=round(100*E802q_count/total_count, 2),PackageCount=E802q_count,TotalSize=sum(netLayer$TotalSize))
  arp_stat <- data.frame(Protocol="ARP",Precentage=round(100*arp_count/total_count, 2),PackageCount=arp_count,TotalSize=sum(arp_filter))
  
  df <- rbind(E802q_stat, arp_stat)
  
  return(df)
}

# return a table format
make_table <- function(data, title){
  table <- tableGrob(data)
  h <- grobHeight(table)
  w <- grobWidth(table)
  title <- textGrob(title, y=unit(0.5,"npc") + h, vjust=0, gp=gpar(fontsize=16))
  return(gTree(children=gList(table, title)))
}

# plots for packet length analysis for bullet point 2
plot_total_packlen_cdf <- function(data, ip_data, non_ip_data, dir){
  x_label <- 'Sample Quantiles of size'
  
  # scatter plot
  if(plot_scatter){
    png(set_filePath(dir, "packlen_total_scatter.png"), height=800, width=1000)
    
    par(mfrow=c(1,1))
    plot(data$frame.len[!is.na(data$frame.len)], xlab=x_label, main="Total length scatter", col="black")
    dev.off();
  }
  
  # ecdf plot
  if(plot_ecdf){
    png(set_filePath(dir, "packlen_total_ecdf.png"), height=800, width=1000)
    
    par(mfrow=c(1,1))
    total_len_ecdf <- ecdf(data$frame.len[!is.na(data$frame.len)])
    plot(total_len_ecdf, xlab=x_label, main="Total length ecdf", col="black")
    dev.off();
  }
  
  # cdf plot
  if(plot_cdf){
    png(set_filePath(dir, "packlen_total_cdf.png"), height=800, width=1000)
    
    par(mfrow=c(1,1))
    total_len_cdf <- cumsum(data$frame.len[!is.na(data$frame.len)])
    plot(total_len_cdf, xlab=x_label, main="Total length cdf", col="black")
    dev.off();
  }
  
  tcp_data <- ip_data$frame.len[ip_data$Protocol=="TCP"]
  udp_data <- ip_data$frame.len[ip_data$Protocol=="UDP"]
  
  # scatter plots
  if(plot_scatter){
    png(set_filePath(dir, "packlen_scatter.png"), height=800, width=1000)
    par(mfrow=c(2,2))
    plot(ip_data$frame.len[!is.na(ip_data$frame.len)], xlab=x_label, main="IP Packet Length scatter", col="red")
    plot(non_ip_data$frame.len[!is.na(non_ip_data$frame.len)], xlab=x_label, main="non-IP Packet Length scatter", col="pink")
    plot(tcp_data[!is.na(tcp_data)], xlab=x_label, main="TCP Packet Length scatter", col="blue")
    plot(udp_data[!is.na(udp_data)], xlab=x_label, main="UDP Packet Length scatter", col="yellow")
    dev.off()
  }
  
  # ecdf plots
  if(plot_ecdf){
    png(set_filePath(dir, "packlen_ecdfs.png"), height=800, width=1000)
    
    ip_len_ecdf <- ecdf(ip_data$frame.len[!is.na(ip_data$frame.len)])
    non_ip_len_ecdf <- ecdf(non_ip_data$frame.len[!is.na(non_ip_data$frame.len)])
    tcp_len_ecdf <- ecdf(tcp_data[!is.na(tcp_data)])
    udp_len_ecdf <- ecdf(udp_data[!is.na(udp_data)])
  
  
    par(mfrow=c(2,2))
    plot(ip_len_ecdf, xlab=x_label, main="IP Packet Length ecdf", col="red")
    plot(non_ip_len_ecdf, xlab=x_label, main="non-IP Packet Length ecdf", col="pink")
    plot(tcp_len_ecdf, xlab=x_label, main="TCP Packet Length ecdf", col="blue")
    plot(udp_len_ecdf, xlab=x_label, main="UDP Packet Length ecdf", col="yellow")
    dev.off()
  }
  
  # cdf plots
  if(plot_cdf){
    png(set_filePath(dir, "packlen_cdfs.png"), height=800, width=1000)
    
    ip_len_cdf <- cumsum(ip_data$frame.len[!is.na(ip_data$frame.len)])
    non_ip_len_cdf <- cumsum(non_ip_data$frame.len[!is.na(non_ip_data$frame.len)])
    tcp_len_cdf <- cumsum(tcp_data[!is.na(tcp_data)])
    udp_len_cdf <- cumsum(udp_data[!is.na(udp_data)])
    
    par(mfrow=c(2,2))
    plot(ip_len_cdf, main="IP header cdf", col="black")
    plot(non_ip_len_cdf, main="Non_IP header cdf", col="pink")
    plot(tcp_len_cdf, main="TCP header cdf", col="blue")
    plot(udp_len_cdf, main="UDP header cdf", col="red")
    dev.off()
  }
}

# plots for header length analysis for bullet point 2
plot_total_headerlen_cdf <- function(data, dir){
  x_label <- 'Sample Quantiles of size'
  
  # scatter plots
  if(plot_scatter){
    png(set_filePath(img_dir, "header_length_scatter.png"), height=800, width=1000)
  
    par(mfrow=c(2,2))
    plot(data$ip.header[!is.na(data$ip.header)], xlab=x_label, main="IP header scatter")
    plot(data$tcp.header[!is.na(data$tcp.header)], xlab=x_label, main="TCP header scatter")
    plot(data$udp.header[!is.na(data$udp.header)], xlab=x_label, main="UDP header scatter")
    dev.off()
  }
  
  # ecdf plots
  if(plot_ecdf){
    png(set_filePath(img_dir, "header_length_ecdfs.png"), height=800, width=1000)
    
    ip_header_ecdf <- ecdf(data$ip.header[!is.na(data$ip.header)])
    tcp_header_ecdf <- ecdf(data$tcp.header[!is.na(data$tcp.header)])
    udp_header_ecdf <- ecdf(data$udp.header[!is.na(data$udp.header)])
    
    par(mfrow=c(2,2))
    plot(ip_header_ecdf, xlab=x_label, main="IP header cdf")
    plot(tcp_header_ecdf, xlab=x_label, main="TCP header cdf")
    plot(udp_header_ecdf, xlab=x_label, main="UDP header cdf")
    dev.off()
  }
  
  # cdf plots
  if(plot_cdf){
    png(set_filePath(img_dir, "header_length_cdfs.png"), height=800, width=1000)
    
    ip_header_cdf <- cumsum(data$ip.header[!is.na(data$ip.header)])
    tcp_header_cdf <- cumsum(data$tcp.header[!is.na(data$tcp.header)])
    udp_header_cdf <- cumsum(data$udp.header[!is.na(data$udp.header)])
    
    par(mfrow=c(2,2))
    plot(ip_header_cdf, main="IP header cdf", col="black")
    plot(tcp_header_cdf,main="TCP header cdf", col="yellow")
    plot(udp_header_cdf, main="UDP header cdf", col="blue")
    dev.off()
  }
}

# Load data
data <- read.table(set_filePath(data_dir, "result.txt"), sep = "\t" , header = TRUE, stringsAsFactors = FALSE)
data$ip.len <- suppressWarnings(as.integer(data$ip.len))

# Process header size
data$ip.header <- c(data$frame.len)-c(data$ip.len)
data$udp.header <- c(data$frame.len)-c(data$udp.length)
data$tcp.header <- c(data$frame.len)-c(data$tcp.len)

ip_filter <- data$X_ws.col.Protocol %in% c('TCP', 'UDP', 'IPv4', 'IPv6')
ip_data <- data.frame(
  Protocol=data$X_ws.col.Protocol[ip_filter],
  frame.len=data$frame.len[ip_filter],
  ip.len=data$ip.len[ip_filter],
  tcp.len=data$tcp.len[ip_filter],
  udp.len=data$udp.length[ip_filter],
  ip.header=data$ip.header[ip_filter],
  udp.header=data$udp.header[ip_filter],
  tcp.header=data$tcp.header[ip_filter]
)

non_ip_data <- data.frame(
  Protocol <- data$X_ws.col.Protocol[data$X_ws.col.Protocol %in% other_net_protocol],
  frame.len <- data$frame.len[data$X_ws.col.Protocol %in% other_net_protocol],
  ip.len <- data$ip.len[data$X_ws.col.Protocol %in% other_net_protocol]
)

# plot per-packet analysis bullet point 1
plot_precentage_tables(data, img_dir)

# plot per-packet analysis bullet point 2
plot_total_packlen_cdf(data, ip_data, non_ip_data, img_dir)
plot_total_headerlen_cdf(ip_data, img_dir)

