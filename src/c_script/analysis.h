#include <net/ethernet.h>
#include <netinet/ip.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <pcap/pcap.h>

int extract_package_info(char *filename);
void printMac(u_char *mac);
int read_ethernet_packet(const u_char *packet);
int read_ip_packet(const u_char *packet, int ether_header_size);
int read_tcp_packet(const u_char *packet, int ip_header_size);
void print_flags(unsigned char flag);