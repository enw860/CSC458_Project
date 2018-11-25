################## begin default settings #####################
x_label <- "packet length"
y_label <- "cummulate probability"
################## end default settings #####################


################## start Extract IP/Non-IP packets #####################
get_IP_packets <- function(all_packets, ip_filter){
  ip_data <- data.frame(
    Protocol=all_packets$X_ws.col.Protocol[ip_filter],
    frame.len=all_packets$frame.len[ip_filter],
    frame.cap_len=all_packets$frame.cap_len[ip_filter],
    ip.total_len=all_packets$ip.len[ip_filter],
    ip.header=all_packets$ip.hdr_len[ip_filter],
    tcp.header=all_packets$tcp.hdr_len[ip_filter],
    tcp.seg_len=all_packets$tcp.len[ip_filter],
    udp.header=all_packets$udp.hdr_size[ip_filter],
    udp.total_len=all_packets$udp.length[ip_filter]
  )
  
  return(ip_data)
}

get_non_IP_packets <- function(all_packets, ip_filter){
  filter <- !ip_filter
  non_ip_data <- data.frame(
    Protocol=all_packets$X_ws.col.Protocol[filter],
    frame.len=all_packets$frame.len[filter],
    frame.cap_len=all_packets$frame.cap_len[filter],
    ip.total_len=all_packets$ip.len[filter],
    ip.header=all_packets$ip.hdr_len[filter]
  )
  return(non_ip_data)
}
################## end Extract IP/Non-IP packets #####################


################## start plot precentage tables #####################
plot_precentage_tables <- function(data, dir){
  link_stat <- link_statistics(data)
  net_stat <- network_statistics(data, link_stat)
  trans_stat <- transport_statistics(data, net_stat)
  
  # plot tables
  png(set_filePath(dir, "packets_precentage_statistics.png"), height=450, width=400)
  grid.arrange(
    make_table(link_stat, "Link Layer Packets Statistics"),
    make_table(net_stat, "Network Layer Packets Statistics"),
    make_table(trans_stat, "Transport Layer Packets Statistics")
  )
  dev.off()
}

# count packets and size of packets in Link Layer
link_statistics <- function(data){
  eth_packet_size <- data$frame.len[!is.na(data$eth.type)]
  other_packet_size <- data$frame.len[is.na(data$eth.type)]
  
  # count number of packets
  eth_count <- length(eth_packet_size) 
  other_count <- length(other_packet_size) 
  
  total_count <- eth_count + other_count
  
  # generate statistics for each terms
  eth_stat <- data.frame(Protocol="Ethernet",Precentage=round(100*eth_count/total_count, 2),PackageCount=eth_count,TotalSize=sum(eth_packet_size))
  other_stat <- data.frame(Protocol="Other",Precentage=round(100*other_count/total_count, 2),PackageCount=other_count,TotalSize=sum(other_packet_size))
  
  # generate statistics for link layer
  df <- rbind(eth_stat, other_stat)
  
  return(df)
}

# count packets and size of packets in Network Layer
network_statistics <- function(data, linkLayer){
  ipv4_packet_size <- data$frame.len[!is.na(data$ip.len)]
  ipv6_packet_size <- data$frame.len[data$ipv6.addr!=""]
  icmpv4_packet_size <- data$frame.len[!is.na(data$icmp.checksum)]
  icmpv6_packet_size <- data$frame.len[!is.na(data$icmpv6.checksum)]
  
  # count number of packets
  icmpv4_count <- length(icmpv4_packet_size) 
  icmpv6_count <- length(icmpv6_packet_size) 
  ipv4_count <- length(ipv4_packet_size) - icmpv4_count
  ipv6_count <- length(ipv6_packet_size) - icmpv6_count
  
  total_count <- linkLayer$PackageCount[linkLayer$Protocol=="Ethernet"]
  
  other_count <- total_count - ipv4_count - ipv6_count - icmpv4_count - icmpv6_count
  other_packet_size <- linkLayer$TotalSize[linkLayer$Protocol=="Ethernet"] - sum(ipv4_packet_size) - sum(ipv6_packet_size) - sum(icmpv4_packet_size) - sum(icmpv6_packet_size)
  
  # generate statistics for each terms
  ipv4_stat <- data.frame(Protocol="IPv4", Precentage=round(100*ipv4_count/total_count, 2), PackageCount=ipv4_count,TotalSize=sum(ipv4_packet_size)-sum(icmpv4_packet_size))
  ipv6_stat <- data.frame(Protocol="IPv6", Precentage=round(100*ipv6_count/total_count, 2), PackageCount=ipv6_count,TotalSize=sum(ipv6_packet_size)-sum(icmpv6_packet_size))
  icmpv4_stat <- data.frame(Protocol="ICMPv4", Precentage=round(100*icmpv4_count/total_count, 2), PackageCount=icmpv4_count,TotalSize=sum(icmpv4_packet_size))
  icmpv6_stat <- data.frame(Protocol="ICMPv6", Precentage=round(100*icmpv6_count/total_count, 2), PackageCount=icmpv6_count,TotalSize=sum(icmpv6_packet_size))
  other_stat <- data.frame(Protocol="Other", Precentage=round(100*other_count/total_count, 2), PackageCount=other_count,TotalSize=other_packet_size)
  
  # generate statistics for network layer
  df <- rbind(ipv4_stat, icmpv4_stat)
  df <- rbind(df, ipv6_stat)
  df <- rbind(df, icmpv6_stat)
  df <- rbind(df, other_stat)
  
  return(df)
}

# count packets and size of packets in Transport Layer
transport_statistics <- function(data, netLayer){
  tcp_packets_size <- data$frame.len[!is.na(data$tcp.len)]
  udp_packets_size <- data$frame.len[!is.na(data$udp.length)]
  
  total_count <- netLayer$PackageCount[netLayer$Protocol=="IPv4"]
  
  # count number of packets
  tcp_count   <- length(tcp_packets_size)
  udp_count   <- length(udp_packets_size)
  
  other_count <- total_count - tcp_count - udp_count
  other_packets_size <- netLayer$TotalSize[netLayer$Protocol=="IPv4"] - sum(tcp_packets_size) - sum(udp_packets_size)
  
  # generate statistics for each terms
  tcp_stat   <- data.frame(Protocol="TCP", Precentage=round(100*tcp_count/total_count, 2),PackageCount=tcp_count,TotalSize=sum(tcp_packets_size))
  udp_stat   <- data.frame(Protocol="UDP", Precentage=round(100*udp_count/total_count, 2), PackageCount=udp_count,TotalSize=sum(udp_packets_size))
  other_stat <- data.frame(Protocol="Other",Precentage=round(100*other_count/total_count, 2), PackageCount=other_count,TotalSize=other_packets_size)
  
  # generate statistics for transport layer
  df <- rbind(tcp_stat, udp_stat)
  df <- rbind(df, other_stat)
  
  return(df)
}
################## end plot precentage tables #####################


################## start plot packet length cdf #####################
plot_total_packlen_cdf <- function(data, ip_data, non_ip_data, dir){
  ### total packet length cdf
  # scatter plot
  if(plot_scatter){
    png(set_filePath(dir, "packlen_total_scatter.png"), height=800, width=1000)
    
    par(mfrow=c(1,1))
    plot(data$frame.len[!is.na(data$frame.len)], xlab="index",  ylab="packet size", main="Total length scatter")
    dev.off();
  }
  
  # ecdf plot
  if(plot_ecdf){
    png(set_filePath(dir, "packlen_total_ecdf.png"), height=800, width=1000)
    
    par(mfrow=c(1,1))
    total_len_ecdf <- ecdf(data$frame.len[!is.na(data$frame.len)])
    plot(total_len_ecdf, xlab=x_label, ylab=y_label, main="Total length ecdf")
    dev.off();
  }
  
  # cdf plot
  if(plot_cdf){
    png(set_filePath(dir, "packlen_total_cdf.png"), height=800, width=1000)
    
    par(mfrow=c(1,1))
    cdf(data$frame.len, "Total length cdf")
    dev.off();
  }
  
  ### packet length for ip, udp, tcp packet length
  tcp_data <- ip_data$frame.len[!is.na(ip_data$tcp.header)]
  udp_data <- ip_data$frame.len[!is.na(ip_data$udp.header)]
  
  # scatter plots
  if(plot_scatter){
    png(set_filePath(dir, "packlen_scatter.png"), height=800, width=1000)
    
    par(mfrow=c(2,2))
    plot(ip_data$frame.len[!is.na(ip_data$frame.len)], xlab="index",  ylab="packet size", main="IP Packet Length scatter")
    plot(non_ip_data$frame.len[!is.na(non_ip_data$frame.len)],  xlab="index",  ylab="packet size", main="non-IP Packet Length scatter")
    plot(tcp_data[!is.na(tcp_data)], xlab="index",  ylab="packet size", main="TCP Packet Length scatter")
    plot(udp_data[!is.na(udp_data)], xlab="index",  ylab="packet size", main="UDP Packet Length scatter")
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
    plot(ip_len_ecdf, xlab=x_label, ylab=y_label, main="IP Packet Length ecdf")
    plot(non_ip_len_ecdf, xlab=x_label, ylab=y_label, main="non-IP Packet Length ecdf")
    plot(tcp_len_ecdf, xlab=x_label, ylab=y_label, main="TCP Packet Length ecdf")
    plot(udp_len_ecdf, xlab=x_label, ylab=y_label, main="UDP Packet Length ecdf")
    dev.off()
  }
  
  # cdf plots
  if(plot_cdf){
    png(set_filePath(dir, "packlen_cdfs.png"), height=800, width=1000)
    
    par(mfrow=c(2,2))
    cdf(ip_data$frame.len, "IP Packet Length cdf")
    cdf(non_ip_data$frame.len, "Non_IP Packet Length cdf")
    cdf(tcp_data[!is.na(tcp_data)], "TCP Packet Length cdf")
    cdf(udp_data[!is.na(udp_data)], "UDP Packet Length cdf")
    dev.off()
  }
}
################## end plot packet length cdf #####################






################## start plot packet hearder length cdf #####################
# plots for header length analysis for bullet point 2
plot_total_headerlen_cdf <- function(data, dir){
  # scatter plots
  if(plot_scatter){
    png(set_filePath(img_dir, "header_length_scatter.png"), height=800, width=1000)
  
    par(mfrow=c(2,2))
    plot(data$ip.header[!is.na(data$ip.header)], xlab="index",  ylab="packet size", main="IP header scatter")
    plot(data$tcp.header[!is.na(data$tcp.header)], xlab="index",  ylab="packet size", main="TCP packet header scatter")
    plot(data$udp.header[!is.na(data$udp.header)], xlab="index",  ylab="packet size", main="UDP packet header scatter")
    dev.off()
  }
  
  # ecdf plots
  if(plot_ecdf){
    png(set_filePath(img_dir, "header_length_ecdfs.png"), height=800, width=1000)
    
    ip_header_ecdf <- ecdf(data$ip.header[!is.na(data$ip.header)])
    tcp_header_ecdf <- ecdf(data$tcp.header[!is.na(data$tcp.header)])
    udp_header_ecdf <- ecdf(data$udp.header[!is.na(data$udp.header)])
    
    par(mfrow=c(2,2))
    plot(ip_header_ecdf, xlab=x_label, ylab=y_label, main="IP packet header cdf")
    plot(tcp_header_ecdf, xlab=x_label, ylab=y_label, main="TCP packet header cdf")
    plot(udp_header_ecdf, xlab=x_label, ylab=y_label, main="UDP packet header cdf")
    dev.off()
  }
  
  # cdf plots
  if(plot_cdf){
    png(set_filePath(img_dir, "header_length_cdfs.png"), height=800, width=1000)
    
    par(mfrow=c(2,2))
    cdf(data$ip.header, "IP packet header cdf")
    cdf(data$tcp.header, "TCP packet header cdf")
    cdf(data$udp.header, "UDP packet header cdf")
    dev.off()
  }
}
################## start plot packet hearder length cdf #####################