library(grid)
library(gridExtra)
library(ggplot2)
library(varhandle)
library(rlist)

################## begin default settings #####################
# default setting (set your own absoulte path)
root_dir <- system("pwd", intern = TRUE)

data_dir <- paste(root_dir, "data/", sep="/")
img_dir <- paste(root_dir, "plots/", sep="/")

# loading sources
source(paste(root_dir, "analysis_perflow/analysis_perflow.R", sep="/"))
source(paste(root_dir, "analysis_perpack/analysis_perpack.R", sep="/"))
source(paste(root_dir, "analysis_RTT/analysis_RTT.R", sep="/"))
source(paste(root_dir, "util/util.R", sep="/"))
################## end default settings #####################

######################### start control box ############
run_perpacket <- bool_switch(1)
run_perflow <- bool_switch(1)
run_rtt <- bool_switch(1)

plot_scatter <- bool_switch(0)
plot_ecdf <- bool_switch(0)
plot_cdf <- bool_switch(1)
######################### end control box ##############

################# start perpacket analysis ##########################
if(run_perpacket){
  # Load data
  print("Loading packets detail")
  all_packets <- read.table(set_filePath(data_dir, "result.tsv"), sep = "\t" , header = TRUE, stringsAsFactors = FALSE)
  
  # cast header length into number
  all_packets$ip.hdr_len <- suppressWarnings(as.integer(all_packets$ip.hdr_len))
  all_packets$ip.len <- suppressWarnings(as.integer(all_packets$ip.len))
  
  # calculate UDP header length
  udp_header_length <- rep(1, nrow(all_packets))
  udp_header_length[is.na(all_packets$udp.length)] <- NA
  
  udp_header_dstport <-  rep(1, nrow(all_packets))
  udp_header_dstport[is.na(all_packets$udp.dstport)] <- NA
  
  udp_header_srcport <-  rep(1, nrow(all_packets))
  udp_header_srcport[is.na(all_packets$udp.srcport)] <- NA
  
  udp_header_checksum <-  rep(1, nrow(all_packets))
  udp_header_checksum[is.na(all_packets$udp.checksum)] <- NA
  
  all_packets$udp.hdr_size <- 2 * (udp_header_length + udp_header_dstport + udp_header_srcport + udp_header_checksum)
  
  ip_filter <- !is.na(all_packets$ip.len) | (all_packets$ipv6.addr!="")
  
  # parsing data into groups
  IP_packets <- get_IP_packets(all_packets, ip_filter)
  non_IP_packets <- get_non_IP_packets(all_packets, ip_filter)
  
  # plot per-packet analysis bullet point 1
  print("Start ploting overall table statistics")
  plot_precentage_tables(all_packets, img_dir)
  
  # plot per-packet analysis bullet point 2
  print("Start ploting package length cdf")
  plot_total_packlen_cdf(all_packets, IP_packets, non_IP_packets, img_dir)
  
  print("Start ploting package header length cdf")
  plot_total_headerlen_cdf(IP_packets, img_dir)
}
################# end perpacket analysis ##########################

################# start perflow analysis ##########################
if(run_perflow){
  # loading data (~ 3 mins)
  print("Loading all tcp packets data")
  tcps <- read.table(set_filePath(data_dir, "tcp_result.tsv"), header=TRUE, sep="\t")
  tcps <- tcps[!is.na(tcps$tcp.hdr_len), c(1:ncol(tcps))]
  tcps$ip.hdr_len <- suppressWarnings(as.numeric(unfactor(tcps$ip.hdr_len)))
  
  print("Loading all udp packets data")
  udps <- read.table(set_filePath(data_dir, "udp_result.tsv"), header=TRUE, sep="\t")
  udps <- udps[!is.na(udps$udp.length), c(1:ncol(udps))]
  udps$ip.hdr_len <- suppressWarnings(as.numeric(unfactor(udps$ip.hdr_len)))
  
  # convert tcp time
  tcps$frame.time <- convert_time(tcps$frame.time) 
  tcps$frame.time <- tcps$frame.time - tcps$frame.time[1]
  
  # convert udp time
  udps$frame.time <- convert_time(udps$frame.time) 
  udps$frame.time <- udps$frame.time - udps$frame.time[1]
  
  # analysis data (~ 8 mins)
  print("Analysis TCP flows (~ 5 mins)")
  overall_tcp_flows <- analysis_overall_tcp_flows(tcps)
  
  print("Analysis UDP flows (~ 3 mins)")
  overall_udp_flows <- analysis_overall_udp_flows(udps)
  
  # per flow bullet point 1
  print("Start ploting TCP and UDP flows summary table")
  flows_statistics(img_dir ,overall_tcp_flows, overall_udp_flows)
  
  # per flow bullet point 2
  print("Start ploting TCP and UDP duration cdf")
  duration_cdfs(img_dir ,overall_tcp_flows, overall_udp_flows)
  
  # per flow bullet point 3
  print("Start ploting TCP and UDP byte sum cdf")
  byteSum_cdfs(img_dir, overall_tcp_flows, overall_udp_flows)
  
  print("Start ploting TCP and UDP packet count cdf")
  packetCount_cdfs(img_dir ,overall_tcp_flows, overall_udp_flows)
  
  print("Start ploting TCP overhead rate cdf")
  overhead_cdfs(img_dir, overall_tcp_flows)
  
  # per flow bullet point 4
  print("Start ploting TCP inter arriving time cdf")
  inter_packet_arrival_cdfs(img_dir ,overall_tcp_flows, overall_udp_flows)
  
  # per flow bullet point 5
  print("Start ploting TCP flows exit state table")
  exit_state_statistics(img_dir, overall_tcp_flows)
}
################# end perflow analysis ##########################

################# start RTT analysis ##########################
if(run_rtt){
  # get top three TCP connection with largest package count
  three_longest_packnum_stream <- head(overall_tcp_flows[order(overall_tcp_flows$packets, decreasing = TRUE), 1], 3)
  print(paste("Top three TCP connection with largest package count: ", three_longest_packnum_stream))
  
  # get top three TCP connection with largest Bytes sum
  three_largest_bytesum_stream <- head(overall_tcp_flows[order(overall_tcp_flows$total_bytes, decreasing = TRUE), 1], 3)
  print(paste("Top three TCP connection with largest Bytes sum: ", three_largest_bytesum_stream))
  
  # get top three TCP connection with longest duration
  three_largest_duration_stream <- head(overall_tcp_flows[order(overall_tcp_flows$duration, decreasing = TRUE), 1], 3)
  print(paste("Top three TCP connection with longest duration: ", three_largest_duration_stream))
  
  # analysis and draw Sample RTT and estimate RTT
  analysis_streams <- sort(unique(c(three_longest_packnum_stream, three_largest_bytesum_stream, three_largest_duration_stream)))

  # plot analysis for all streams in analysis streams
  print(paste("analysis TCP connection: ", analysis_streams))
  analysis_all_stream(img_dir, tcps, analysis_streams)
  
  # plot analysis for median streams for top three TCP connections
  print("Analysis 3 hosts TCP connection over time")
  analysis_median_host_stream(img_dir, tcps, overall_tcp_flows, 3)
}
################# end RTT analysis ##########################

