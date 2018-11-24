################# begin analusis RRT ########################
analysis_all_stream <- function(img_dir, all_stream, stream_num_list){
  for(stream  in stream_num_list)  {
    print(paste("ploting flow #",stream, sep=""))
    
    analysis_tcp_stream(img_dir, all_stream, stream)
  }
}

analysis_median_host_stream <- function(img_dir, all_stream, overall_tcp_flows, n=3){
  stream_lists <- streams_of_top_paired_hosts(overall_tcp_flows, n)
  for(i in c(1:n)){
    print(paste("ploting median analysis #",i, sep=""))
    
    statics <- mean_rrt_tcp_stream(img_dir, all_stream, stream_lists[[i]])
    plot_median_analysis(img_dir, statics, i)
  }
}

analysis_tcp_stream <- function(img_dir, all_stream, stream_num){
  stream <- extract_stream(all_stream, stream_num)
  #stream <- build_RRT(stream)
  
  png(paste(img_dir, "stream", stream_num,".png", sep=""), height=800, width=1000)
  
  par(mfrow=c(2, 2))
  sample_rrt_dist(stream)
  estimate_rrt_dist(stream)
  
  dev.off()
}

mean_rrt_tcp_stream <- function(img_dir, all_stream, stream_num_list){
  acc <- NULL
  
  for(stream_num in stream_num_list){
    acc <- rbind(acc, extract_stream_media(all_stream, stream_num))
  }
  
  return(acc)
}

extract_stream <- function(all_stream, stream_num){
  # stream filter
  stream_filter <- all_stream$tcp.stream == stream_num
  
  # count connection duration
  arrival_time <- all_stream$frame.time[stream_filter]
  
  tcp_flow <- data.frame(
    ip_dest=paste(all_stream$ip.dst[stream_filter], all_stream$tcp.dstport[stream_filter], sep = ":"),
    source=paste(all_stream$ip.src[stream_filter], all_stream$tcp.srcport[stream_filter], sep = ":"),
    ack=all_stream$tcp.ack[stream_filter],
    seq=all_stream$tcp.nxtseq[stream_filter],
    arriving_time=arrival_time,
    ack_rrt=all_stream$tcp.analysis.ack_rtt[stream_filter]
  )
  
  tcp_flow$ack_type <- "BAB"
  tcp_flow$ack_type[tcp_flow$ip_dest == tcp_flow$ip_dest[1]] <- "ABA"
  
  return(tcp_flow)
}

build_RRT <- function(tcp_stream){
  tcp_stream$rrt <- NA
  
  progress <- 0
  for(i in c(1:nrow(tcp_stream))){
    tcp_stream <- find_ack_rtt(tcp_stream, i) 
    
    if(as.integer(i*100/nrow(tcp_stream)) > progress+9){
      progress <- as.integer(i*100/nrow(tcp_stream))
      print(paste(progress,"% done", sep=""))
    }
  }
  
  return(tcp_stream)
}

find_ack_rtt <- function(tcp_stream, num){
  current_tcp_pack <- tcp_stream[num, c(1:ncol(tcp_stream))]
  
  # case when lost packet happen
  if(sum(tcp_stream$seq == current_tcp_pack$seq) != 1){
    return(tcp_stream)
  }
  
  filter <- tcp_stream$arriving_time > current_tcp_pack$arriving_time & tcp_stream$ack==current_tcp_pack$seq
  
  back_traffic <- tcp_stream[filter, c(1:ncol(tcp_stream))]$arriving_time
  
  if(length(back_traffic) > 0){
    tcp_stream[filter, c(1:ncol(tcp_stream))]$rrt[1] <- back_traffic[1] - current_tcp_pack$arriving_time
  }
  
  return(tcp_stream)
}

sample_rrt_dist <- function(stream){
  ABA_arriving_time <- stream$arriving_time[stream$ack_type=="ABA"]
  ABA_rrt <- stream$ack_rrt[stream$ack_type=="ABA"]
  
  BAB_arriving_time <- stream$arriving_time[stream$ack_type=="BAB"]
  BAB_rrt <- stream$ack_rrt[stream$ack_type=="BAB"]
  
  y_limit <- max(c(0.001, ABA_rrt[!is.na(ABA_rrt)], BAB_rrt[!is.na(BAB_rrt)])) * 1.05
  
  if(sum(!is.na(ABA_rrt))==0){
    plot(0, 0, main="Sample RTT from A to B", ylim=c(0, y_limit), xlab="Arriving time", ylab="RTT", type="l", col="blue")
  }else{
    plot(ABA_arriving_time[!is.na(ABA_rrt)], ABA_rrt[!is.na(ABA_rrt)], main="Sample RTT from A to B", ylim=c(0, y_limit), xlab="Arriving time", ylab="RTT", type="l", col="blue")
  }
  
  if(sum(!is.na(BAB_rrt))==0){
    plot(0, 0, main="Sample RTT from B to A", ylim=c(0, y_limit), xlab="Arriving time", ylab="RTT", type="l", col="red")
  }else{
    plot(BAB_arriving_time[!is.na(BAB_rrt)], BAB_rrt[!is.na(BAB_rrt)], main="Sample RTT from B to A", ylim=c(0, y_limit), xlab="Arriving time", ylab="RTT", type="l", col="red")
  }
}

estimate_rrt_dist <- function(stream){
  ABA_arriving_time <- stream$arriving_time[stream$ack_type=="ABA"]
  ABA_rrt <- stream$ack_rrt[stream$ack_type=="ABA"]
  if(sum(!is.na(ABA_rrt))==0){
    ABA_srrt <- NULL
  }else{
    ABA_srrt <- compute_SRTT(ABA_rrt[!is.na(ABA_rrt)])
  }
  
  BAB_arriving_time <- stream$arriving_time[stream$ack_type=="BAB"]
  BAB_rrt <- stream$ack_rrt[stream$ack_type=="BAB"]
  if(sum(!is.na(BAB_rrt))==0){
    BAB_srrt <- NULL
  }else{
    BAB_srrt <- compute_SRTT(BAB_rrt[!is.na(BAB_rrt)])  
  }
  
  y_limit <- max(c(0.001, ABA_srrt, BAB_srrt))*1.05
  
  if(sum(!is.na(ABA_rrt))==0){
    plot(0, 0, main="Estimate RTT from A to B", ylim=c(0, y_limit), xlab="Arriving time", ylab="RTT", type="l", col="blue")
  }else{
    plot(ABA_arriving_time[!is.na(ABA_rrt)], ABA_srrt, ylim=c(0, y_limit), main="Estimate RTT from A to B", xlab="Arriving time", ylab="RTT", type="l", col="blue")
  }
  
  if(sum(!is.na(BAB_rrt))==0){
    plot(0, 0, main="Estimate RTT from B to A", ylim=c(0, y_limit), xlab="Arriving time", ylab="RTT", type="l", col="red")
  }else{
    plot(BAB_arriving_time[!is.na(BAB_rrt)], BAB_srrt, ylim=c(0, y_limit), main="Estimate RTT from B to A", xlab="Arriving time", ylab="RTT", type="l", col="red")
  }
}

# RFC 6298 RTT estimation
compute_SRTT <- function(rrt, alpha=1/8, beta=1/4){
  R <- rrt[1]
  
  # initial value
  SRTT <- R
  RTTVAR <- R/2
  ret <- c(SRTT)
  
  for(r in rrt[-1]){
    R <- r
    RTTVAR <- (1 - beta) * RTTVAR + beta * abs(SRTT - R)
    SRTT <- (1 - alpha) * SRTT + alpha * R
    ret <- c(ret, SRTT)
  }
  return(ret)
}

streams_of_top_paired_hosts <- function(tcp_flows, n){
  top_pairs <- names(head(sort(table(paste(tcp_flows$dest_ip, tcp_flows$src_ip, sep=":")), decreasing=TRUE), n))
  
  ret <- NULL
  split_result <- strsplit(top_pairs, ":")
  
  for(i in c(1:n)){
    dest <- split_result[[i]][1]
    src <- split_result[[i]][2]
    
    filter <- tcp_flows$dest_ip==dest & tcp_flows$src_ip==src
    ret[[i]] <- tcp_flows$Stream[filter]
  }
  
  return(ret)
}

extract_stream_media <- function(all_stream, stream_num){
  # stream filter
  stream_filter <- all_stream$tcp.stream == stream_num
  tcp_flow <- all_stream[stream_filter, c(1:ncol(all_stream))]
  
  tcp_flow$ack_type <- "BAB"
  tcp_flow$ack_type[tcp_flow$ip.dst == tcp_flow$ip.dst[1]] <- "ABA"
  
  ABAs <- tcp_flow[tcp_flow$ack_type=="ABA", c(1:ncol(tcp_flow))]
  ABAs <- ABAs[!is.na(ABAs$tcp.analysis.ack_rtt), c(1:ncol(ABAs))]
  ABA_median <- data.frame(
    Stream=stream_num,
    ack_type="ABA", 
    arriving_time=median(ABAs$frame.time),
    ack_rrt=median(ABAs$tcp.analysis.ack_rtt)
    #est_rrt=median(compute_SRTT(ABAs$tcp.analysis.ack_rtt))
  )
  
  BABs <- tcp_flow[tcp_flow$ack_type=="BAB", c(1:ncol(tcp_flow))]
  BABs <- BABs[!is.na(BABs$tcp.analysis.ack_rtt), c(1:ncol(BABs))]
  BAB_median <- data.frame(
    Stream=stream_num,
    ack_type="BAB", 
    arriving_time=median(BABs$frame.time),
    ack_rrt=median(BABs$tcp.analysis.ack_rtt)
    #est_rrt=median(compute_SRTT(BABs$tcp.analysis.ack_rtt))
  )
  
  return(rbind(ABA_median, BAB_median))
}

plot_median_analysis <- function(img_dir, streams, n=0){
  png(paste(img_dir, "median_stream", n,".png", sep=""), height=800, width=1000)

  par(mfrow=c(2,2))
  sample_rrt_dist(streams)
  estimate_rrt_dist(streams)
  dev.off()
}
################# end analusis RRT ########################