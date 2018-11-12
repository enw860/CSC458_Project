#include <stdio.h>
#include <stdlib.h>
#include <net/ethernet.h>
#include <netinet/ip.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <pcap/pcap.h>
#include <sys/time.h>
#include "analysis.h"

const u_short ETHER_Q = 0x0081;

int extract_package_info(char *filename){
  //error buffer
  char *errbuff = malloc(sizeof(char) * PCAP_ERRBUF_SIZE);

  pcap_t *handler = pcap_open_offline(filename, errbuff);

  struct pcap_pkthdr *header;
  const u_char *packet;
  int packct_count = 0;

  // general packet information
  while(pcap_next_ex(handler, &header, &packet)){
    if(packct_count == 3) break;

    // link layer
    printf("Package number: %d\n", packct_count++);
    printf("package max_len: %u, package caplen: %u ", header->len, header->caplen);
    printf("Time: %ld:%d\n", header->ts.tv_sec, header->ts.tv_usec);

    int header_size = read_ethernet_packet(packet);
    printf("Header Size: %d, Payload Size: %d\n", header_size, header->caplen - header_size);
    printf("\n");
  }

  free(errbuff);
  return 0;
}

void printMacAddr(u_char *mac){
  int i;
  for(i=0; i<ETHER_ADDR_LEN; i++){
    printf("%02x ", mac[i]);
  }
}

int read_ethernet_packet(const u_char *packet){
  // Ethernet packet
  struct ether_header *ether_header = (struct ether_header *)packet;
  printf("\tEthernet Header: (type: %.4x)\n", ether_header->ether_type);
  
  int ether_header_size = sizeof(struct ether_header);
  if(ether_header->ether_type == ETHER_Q) {
    ether_header_size += 4;
    printf("\tHost Mac: ");
    printMacAddr(ether_header->ether_dhost);
    printMacAddr(ether_header->ether_shost);
    printf("\n");

    printf("\tSender Mac: ");
    u_char *ether_destMAC = (u_char *)(packet+ether_header_size);
    printMacAddr(ether_destMAC);
    printf("\n");
  }else{
    printf("\tHost Mac: ");
    printMacAddr(ether_header->ether_dhost);
    printf("\n");

    printf("\tSender Mac: ");
    printMacAddr(ether_header->ether_shost);
    printf("\n");
  };
  printf("\tEthernet Header Size %d\n", ether_header_size);

  return read_ip_packet(packet, ether_header_size);
}

int read_ip_packet(const u_char *packet, int ether_header_size){
  // IP Packet
  struct ip *ip_header = (struct ip *)(packet+ether_header_size);
  printf("\t\tIP header\n");
  //? why, shouln't the length be 32?
  printf("\t\tHeader length: %02x\n", ip_header->ip_hl);
  printf("\t\tVersion: %02x\n", ip_header->ip_v);
  printf("\t\tType of service: %02x\n", ip_header->ip_tos);
  printf("\t\tTotal length: %.4x\n", ip_header->ip_len);
  printf("\t\tIdentification: %.4x\n", ip_header->ip_id);
  printf("\t\tFrag_offset: %.4x\n", ip_header->ip_off);
  printf("\t\tTime To Live: %02x\n", ip_header->ip_ttl);
  printf("\t\tProtocal: %02x\n", ip_header->ip_p);
  printf("\t\tChecksum: %.4x\n", ip_header->ip_sum);
  printf("\t\tSrc IP: %s ", inet_ntoa(ip_header->ip_src));
  printf("Dest IP: %s\n", inet_ntoa(ip_header->ip_dst));
  printf("\t\tIP Header Size %lu\n", sizeof(struct ip));

  return read_tcp_packet(packet, ether_header_size + sizeof(struct ip));
}

int read_tcp_packet(const u_char *packet, int ip_header_size){
  // TCP Packet 
  printf("\t\t\tTCP header\n");
  struct tcphdr *tcp_header = (struct tcphdr *)(packet+ip_header_size);
  printf("\t\t\tsrc port: %hu ", ntohs(tcp_header->th_sport));
  printf("dest port: %hu \n", ntohs(tcp_header->th_dport));
  printf("\t\t\tSeq No: %u\n", ntohl(tcp_header->th_seq));
  printf("\t\t\tACK No: %u\n", ntohl(tcp_header->th_ack));
  print_flags(tcp_header->th_flags);
  printf("\t\t\tWindow Size: %hu\n", ntohs(tcp_header->th_win));
  printf("\t\t\tChecksum: %hu\n", ntohs(tcp_header->th_sum));
  printf("\t\t\tUrgent: %hu\n", ntohs(tcp_header->th_urp));
  
  int header_size = ip_header_size + sizeof(struct tcphdr);
  return header_size;
}

void print_flags(unsigned char flag){
  if(flag & TH_FIN){
    printf("\t\t\t 1.......: FIN set\n");
  }else{
    printf("\t\t\t 0.......: FIN not set\n");
  }

  if(flag & TH_SYN){
    printf("\t\t\t .1......: SYN set\n");
  }else{
    printf("\t\t\t .0......: SYN not set\n");
  }

  if(flag & TH_RST){
    printf("\t\t\t ..1.....: RST set\n");
  }else{
    printf("\t\t\t ..0.....: RST not set\n");
  }

  if(flag & TH_PUSH){
    printf("\t\t\t ...1....: PUSH set\n");
  }else{
    printf("\t\t\t ...0....: PUSH not set\n");
  }

  if(flag & TH_ACK){
    printf("\t\t\t ....1...: ACK set\n");
  }else{
    printf("\t\t\t ....0...: ACK not set\n");
  }

  if(flag & TH_URG){
    printf("\t\t\t .....1..: URG set\n");
  }else{
    printf("\t\t\t .....0..: URG not set\n");
  }

  if(flag & TH_ECE){
    printf("\t\t\t ......1.: ECE set\n");
  }else{
    printf("\t\t\t ......0.: ECE not set\n");
  }

  if(flag & TH_CWR){
    printf("\t\t\t .......1: CWR set\n");
  }else{
    printf("\t\t\t .......0: CWR not set\n");
  }
}
