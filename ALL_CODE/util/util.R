set_filePath <- function(dir, filename){
  return(paste(dir,filename, sep=""))
}

bool_switch <- function(num){
  if(num == 0){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

# return a table format
make_table <- function(data, title){
  table <- tableGrob(data)
  h <- grobHeight(table)
  w <- grobWidth(table)
  title <- textGrob(title, y=unit(0.5,"npc") + h, vjust=0, gp=gpar(fontsize=16))
  return(gTree(children=gList(table, title)))
}

cdf <- function(data, title, x_label="packet length", y_label="cummulate probability"){
  data <- data[!is.na(data)]
  
  precent_tb <- table(data)/length(data)
  precenatage <- cumsum(rep(unname(precent_tb)))
  packet_length <- (as.integer(names(precent_tb)))
  
  min_packlen <- min(packet_length)
  max_packlen <- max(packet_length)
  
  plot(packet_length, precenatage, main=title, xlab=x_label, ylab=y_label, col="red", type="p", xlim=c(min_packlen, max_packlen), ylim=c(0,1))
  lines(packet_length, precenatage, col="black")
  abline(h = 0, lty = 2)
  abline(h = 1, lty = 2)
}

convert_time <- function(timestamp){
  minutes <- as.integer(substr(timestamp, 17, 18))
  seconds <- as.double(substr(timestamp, 20, 31))
  return(minutes * 60 + seconds)
}