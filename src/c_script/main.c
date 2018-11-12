#include <stdio.h>
#include <stdlib.h>
#include "analysis.h"
#define MAX_PATH_LENGTH 100

const int UTORid1 = 1003002289;
const int UTORid2 = 998325539;
const int DATA_FILE_NO = (UTORid1 + UTORid2) % 20 + 1;
const char *DATA_FILE_PREFIX = "./data/univ1_pt";

int main(int argc, char **argv){
  // initilize the program
  char *file_path = malloc(sizeof(char) * MAX_PATH_LENGTH);

  // print out the path of the data file
  sprintf(file_path, "%s%d", DATA_FILE_PREFIX, DATA_FILE_NO);

  // analysis the pcap file
  extract_package_info(file_path);

  // cleaning up head
  free(file_path);

  return 0;
}