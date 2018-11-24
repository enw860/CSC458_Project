################## begin analysis overall functions #####################
analysis_overall_tcp_flows <- function(tcps){
  overall_tcp_flows <- NULL
  flow_num <- as.integer(levels(factor(tcps$tcp.stream)))
  
  progress <- 0
  for(flow in flow_num){
    flow_set <- tcps[tcps$tcp.stream == flow, c(1:ncol(tcps))]
    overall_tcp_flows <- rbind(overall_tcp_flows, adjust_stream(flow_set))
    
    if(as.integer(flow*100/max(flow_num)) > progress+2){
      progress <- as.integer(flow*100/max(flow_num))
      print(paste("Finish TCP %:", progress))
    }
  }
  
  return(overall_tcp_flows)
}

adjust_stream <- function(stream_data){
  # fix bit status
  stream_data$tcp.connection.fin[is.na(stream_data$tcp.connection.fin)] <- 0
  stream_data$tcp.connection.rst[is.na(stream_data$tcp.connection.rst)] <- 0
  stream_data$tcp.connection.syn[is.na(stream_data$tcp.connection.syn)] <- 0
  
  return(flow_cut(stream_data))
}

flow_cut <- function(stream_data){
  temp_stream_flows <- NULL
  
  syn_index <- which(stream_data$tcp.connection.syn > 0)
  fin_index <- which(stream_data$tcp.connection.fin > 0)
  
  if(length(syn_index) > 1){
    for(i in c(1:length(syn_index)-1)){
      fetchCut <- sum(fin_index>syn_index[i] & fin_index<syn_index[i+1])
      if(fetchCut > 0){
        cutPoint <- syn_index[i+1]-1
        subStream <- stream_data[c(i:cutPoint), nclo(stream_data)]
        temp_stream_flows <- rbind(temp_stream_flows, analysis_tcp_flow(subStream))
      }
    }
  }else{
    temp_stream_flows <- analysis_tcp_flow(stream_data)
  }
  
  return(temp_stream_flows)
}

analysis_tcp_flow <- function(data){
  # count connection duration
  arrival_time <- data$frame.time
  arrival_time <- arrival_time - arrival_time[1]
  
  tcp_flow <- data.frame(
    Stream=data$tcp.stream[1],
    Protocol=data$X_ws.col.Protocol[1],
    dest_ip=data$ip.dst[1], 
    dest_port=data$tcp.dstport[1],
    src_ip=data$ip.src[1], 
    src_port=data$tcp.srcport[1],
    total_hdr_size=sum(data$tcp.hdr_len + data$ip.hdr_len + 18),
    total_bytes=sum(data$frame.len),
    duration=tail(arrival_time, 1) - head(arrival_time, 1),
    avg_inter_arriving=inter_arriving_time(arrival_time),
    packets=length(arrival_time)
  )
  
  # check the exit status
  exit_state <- "ONGOING"
  check_num <- min(3, nrow(data)-1)
  for(i in c(0:check_num)){
    if(data$tcp.connection.fin[nrow(data)-i] == 1){
      exit_state <- "FIN"
      break
    }else if(data$tcp.connection.syn[nrow(data)-i] == 1){
      exit_state <- "SYN"
      break
    }else if(data$tcp.connection.rst[nrow(data)-i] == 1){
      exit_state <- "RST"
      break
    }
  }
  
  tcp_flow$exit_state <- exit_state
  tcp_flow$transfer_Bytes <- tcp_flow$total_bytes - tcp_flow$total_hdr_size
  tcp_flow$transfer_Bytes[tcp_flow$transfer_Bytes == 0] <- 9999
  tcp_flow$overhead_rate <- (tcp_flow$total_hdr_size*100)/ tcp_flow$transfer_Bytes
  
  return(tcp_flow)
}

analysis_overall_udp_flows <- function(udps){
  overall_udp_flows <- NULL
  flow_num <- as.integer(levels(factor(udps$udp.stream)))
  
  progress <- 0
  for(flow in flow_num){
    flow_set <- udps[udps$udp.stream == flow, c(1:ncol(udps))]
    overall_udp_flows <- rbind(overall_udp_flows, analysis_udp_flow(flow_set))
    
    if(as.integer(flow*100/max(flow_num)) > progress+5){
      progress <- as.integer(flow*100/max(flow_num))
      print(paste("Finish UDP %:", progress))
    }
  }
  
  return(overall_udp_flows)
}

analysis_udp_flow <- function(data){
  # count connection duration
  arrival_time <- data$frame.time
  
  udp_flow <- data.frame(
    Stream=data$udp.stream[1],
    Protocol=data$X_ws.col.Protocol[1],
    dest_ip=data$ip.dst[1], 
    dest_port=data$udp.dstport[1],
    src_ip=data$ip.src[1], 
    src_port=data$udp.srcport[1],
    total_hdr_size=sum(8 + data$ip.hdr_len + 18),
    total_bytes=sum(data$frame.len),
    duration=tail(arrival_time, 1) - head(arrival_time, 1),
    avg_inter_arriving=inter_arriving_time(arrival_time),
    packets=length(arrival_time)
  )
  
  udp_flow$transfer_Bytes <- udp_flow$total_bytes - udp_flow$total_hdr_size
  udp_flow$transfer_Bytes[udp_flow$transfer_Bytes == 0] <- 9999
  udp_flow$overhead_rate <- (udp_flow$total_hdr_size*100)/ udp_flow$transfer_Bytes
  
  return(udp_flow)
}

inter_arriving_time <- function(arrival_time){
  inter_arriving <- arrival_time[-1] - arrival_time[-1*length(arrival_time)]
  return(mean(inter_arriving))
}

################## end analysis overall functions #####################

################# start plot grapg ##################
# flows statistics
flows_statistics <- function(dir, tcp_flows, udp_flows){
  tcp_count <- length(tcp_flows$packets)
  udp_count <- length(udp_flows$packets)
  
  total_count <- tcp_count + udp_count
  
  tcp_stat <- data.frame(Protocol="TCP", Precentage=round(100*tcp_count/total_count, 2), FlowCount=tcp_count, TotalPackets=sum(tcp_flows$packets))
  udp_stat <- data.frame(Protocol="UDP", Precentage=round(100*udp_count/total_count, 2), FlowCount=udp_count, TotalPackets=sum(udp_flows$packets))
  
  df <- rbind(tcp_stat, udp_stat)
  
  png(set_filePath(dir, "flows_statistics.png"), height=100, width=400)
  grid.arrange(
    make_table(df, "Flows Statistics")
  )
  dev.off()
}

duration_cdfs <- function(dir ,tcp_flows, udp_flows){
  x_label <- "flows duration(ms)"
  
  tcp_duration <- tcp_flows$duration
  udp_duration <- udp_flows$duration
  total_duration <- c(tcp_duration, udp_duration)
  
  png(set_filePath(dir, "durations_cdfs.png"), height=800, width=1000)
  
  par(mfrow=c(2,2))
  cdf(total_duration, "Total flows duration cdf", x_label)
  cdf(tcp_duration, "TCP flows duration cdf", x_label)
  cdf(udp_duration, "UDP flows duration cdf", x_label)
  dev.off()
}

byteSum_cdfs <- function(dir ,tcp_flows, udp_flows){
  x_label <- "flows bytes"
  
  tcp_bytes <- tcp_flows$total_bytes
  udp_bytes <- udp_flows$total_bytes
  total_bytes <- c(tcp_bytes, udp_bytes)
  
  png(set_filePath(dir, "bytes_cdfs.png"), height=800, width=1000)
  
  par(mfrow=c(2,2))
  cdf(total_bytes, "Total flows bytes cdf", x_label)
  cdf(tcp_bytes, "TCP flows bytes cdf", x_label)
  cdf(udp_bytes, "UDP flows bytes cdf", x_label)
  dev.off()
}

packetCount_cdfs <- function(dir ,tcp_flows, udp_flows){
  x_label <- "flows packets count"
  
  tcp_count <- tcp_flows$packets
  udp_count <- udp_flows$packets
  total_count <- c(tcp_count, udp_count)
  
  png(set_filePath(dir, "count_cdfs.png"), height=800, width=1000)
  
  par(mfrow=c(2,2))
  cdf(total_count, "Total flows packets count cdf", x_label)
  cdf(tcp_count, "TCP flows packets count cdf", x_label)
  cdf(udp_count, "UDP flows packets count cdf", x_label)
  dev.off()
}

overhead_cdfs <- function(dir ,tcp_flows){
  x_label <- "overhead rate %"
  
  tcp_overhead_rate <- tcp_flows$overhead_rate
  
  png(set_filePath(dir, "overhead_cdfs.png"), height=800, width=1000)
  par(mfrow=c(1,1))
  cdf(tcp_overhead_rate, "TCP overhead rate cdf", x_label)
  dev.off()
}

inter_packet_arrival_cdfs <- function(dir ,tcp_flows, udp_flows){
  x_label <- "avgrage inter arriving time (s)"
  
  tcp_avg_inter_arriving <- tcp_flows$avg_inter_arriving[!is.na(tcp_flows$avg_inter_arriving)]
  udp_avg_inter_arriving <- udp_flows$avg_inter_arriving[!is.na(udp_flows$avg_inter_arriving)]
  
  png(set_filePath(dir, "avg_inter_arriving.png"), height=800, width=1000)
  par(mfrow=c(1,2))
  cdf(tcp_avg_inter_arriving, "TCP avgrage inter arriving time", x_label)
  cdf(udp_avg_inter_arriving, "UDP avgrage inter arriving time", x_label)
  dev.off()
}

exit_state_statistics <- function(img_dir, tcp_flows){
  len <- length(tcp_flows$exit_state)
  count_tb <- table(tcp_flows$exit_state)
  
  types <- names(count_tb)
  count <- rep(unname(count_tb))
  precenatage <- round(count*100/len, 2)
  
  stats <- data.frame(types=types, precenatage=precenatage, count=count)

  png(set_filePath(img_dir, "exit_state_statistics.png"), height=300, width=400)
  par(mfrow=c(1,1))
  grid.arrange(
    make_table(stats, "Exit State Statistics")
  )
  dev.off()
}
################# end plot grapg ##################