Description:
	Files in this directory are the coding part for CSC458 course project

Envirement:
	OS envirement: 				MacOS / Linuxs
	Coding language: 			Shell/Bash, R(version 3.4.4)
	Required R Libraries: 		grid, gridExtra, ggplot2, varhandle, rlist
	Network Analysising Tool: 	tShark(version 2.6.4)

Executing Steps:
	1. Please download data zip from following link: http://pages.cs.wisc.edu/~tbenson/IMC_DATA/univ1_trace.tgz. Then unzip all files from univ1_trace.tgz into ./data. 
	2. run `./convert_data` (~ 3 mins)
	3. make sure you have all mentioned R library
	4. run `Rscript ./main.R` (~ 10 mins)
	5. all plots will appear in ./plots

Image naming and description:
	Per-packet analysis:
		- ./plots/packets_precentage_statistics.png
			=> Type of packet in link layer, network layer, and transport layer. Displaying the results as a table, both in terms of count and percentage of all packets and the total number of bytes for listed protocols.

		- ./plots/packlen_total_cdf.png
			=> Cumulative Distribution Function(CDF) of the size of all packets

		- ./plots/packlen_cdfs.png
			=> CDF of the size of packets in group of IP, non-IP, TCP, UDP

		- ./plots/header_length_cdfs.png
			=> CDF of the header size of packets in group of IP, non-IP, TCP, UDP

	Per-flow analysis:
		- ./plots/flows_statistics.png
			=> summary report about tcp and udp flows

		- ./plots/durations_cdfs.png
			=> Flow duration CDF of all ip flows, tcp flows and udp flows

		- ./plots/byteSum_cdfs.png
			=> Flow Bytes Sum CDF of all ip flows, tcp flows and udp flows

		- ./plots/packetCount_cdfs.png
			=> Flow Packets count CDF of all ip flows, tcp flows and udp flows		
		- ./plots/overhead_cdfs.png
			=> TCP flow's overhead ratio CDF

		- ./plots/inter_arriving.png
			=> Inter arriving time cdf for both TCP flows and UDP flows

		- ./plots/TCP_exitState_statistics.png
			=> table that aggregrates all TCP flows exit state

	RTT Estimation:
		- ./plots/stream<TCP Steam Number>.png
			=> Sample RTT and estimate RTT analysis for specific TCP conversation with the steam number

		- ./plots/median_stream<numb>.png
			=> Median sample RTT and median estimate RTT analysis between a pair of hosts who has most TCP conversations between them, 'numb' refers to rank

Others: 
	a log of compiling `Rscript ./main.R` is in file ./log.txt


