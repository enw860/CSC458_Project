library(grid)
library(gridExtra)
library(ggplot2)
library(varhandle)
library(rlist)

################## begin default settings #####################
# default setting (set your own absoulte path)
#root_dir <- "/Users/lionel/Desktop/CSC458/Assignment/project_due_1125/src"
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
  all_packets <- read.table(set_filePath(data_dir, "result.tsv"), sep = "\t" , header = TRUE, stringsAsFactors = FALSE)
  all_packets$ip.hdr_len <- suppressWarnings(as.integer(all_packets$ip.hdr_len))
  all_packets$ip.len <- suppressWarnings(as.integer(all_packets$ip.len))
  
  # parse data
  ip_filter <- all_packets$X_ws.col.Protocol %in% c('TCP', 'UDP', 'IPv4', 'IPv6')
  IP_packets <- get_IP_packets(all_packets, ip_filter)
  non_IP_packets <- get_non_IP_packets(all_packets, !ip_filter)
  
  # plot per-packet analysis bullet point 1
  plot_precentage_tables(all_packets, img_dir)
  
  # plot per-packet analysis bullet point 2
  plot_total_packlen_cdf(all_packets, IP_packets, non_IP_packets, img_dir)
  plot_total_headerlen_cdf(IP_packets, img_dir)
}
################# end perpacket analysis ##########################

################# start perflow analysis ##########################
if(run_perflow){
  # loading data (~ 3 mins)
  tcps <- read.table(set_filePath(data_dir, "tcp_result.tsv"), header=TRUE, sep="\t")
  udps <- read.table(set_filePath(data_dir, "udp_result.tsv"), header=TRUE, sep="\t")
  
  # convert tcp time
  tcps$frame.time <- convert_time(tcps$frame.time) 
  tcps$frame.time <- tcps$frame.time - tcps$frame.time[1]
  
  # convert udp time
  udps$frame.time <- convert_time(udps$frame.time) 
  udps$frame.time <- udps$frame.time - udps$frame.time[1]
  
  # analysis data (~ 5 mins)
  overall_tcp_flows <- analysis_overall_tcp_flows(tcps)
  overall_udp_flows <- analysis_overall_udp_flows(udps)
  
  # per flow bullet point 1
  flows_statistics(img_dir ,overall_tcp_flows, overall_udp_flows)
  
  # per flow bullet point 2
  duration_cdfs(img_dir ,overall_tcp_flows, overall_udp_flows)
  
  # per flow bullet point 3
  byteSum_cdfs(img_dir ,overall_tcp_flows, overall_udp_flows)
  packetCount_cdfs(img_dir ,overall_tcp_flows, overall_udp_flows)
  overhead_cdfs(img_dir, overall_tcp_flows)
  
  # per flow bullet point 4
  inter_packet_arrival_cdfs(img_dir ,overall_tcp_flows, overall_udp_flows)
  
  # per flow bullet point 5
  exit_state_statistics(img_dir, overall_tcp_flows)
}
################# end perflow analysis ##########################

################# start RTT analysis ##########################
if(run_rtt){
  three_longest_packnum_stream <- head(overall_tcp_flows[order(overall_tcp_flows$packets, decreasing = TRUE), 1], 3)
  three_largest_bytesum_stream <- head(overall_tcp_flows[order(overall_tcp_flows$total_bytes, decreasing = TRUE), 1], 3)
  three_largest_duration_stream <- head(overall_tcp_flows[order(overall_tcp_flows$duration, decreasing = TRUE), 1], 3)

  # analysis and draw Sample RTT and estimate RTT
  analysis_streams <- sort(unique(c(three_longest_packnum_stream, three_largest_bytesum_stream, three_largest_duration_stream)))

  # plot analysis for all streams in analysis streams
  analysis_all_stream(img_dir, tcps, analysis_streams)
  
  # plot analysis for median streams for top three TCP connections
  analysis_median_host_stream(img_dir, tcps, overall_tcp_flows, 3)
}
################# end RTT analysis ##########################
