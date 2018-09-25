### SCORING FUNCTION ###

scoreTests <- function(path1, path2, key){
  
  # get number of variables
  nc <- length(testkey) 
  num <- c(1:nc)
  
  # import data
  
  # pretest
  pre <- read.csv(path1, sep=";",stringsAsFactors=FALSE)
  pre <- pre [c(-1)]
  
  # rename variables
  new_names_pre <- c(paste0("q",num),"id1","id2","id3","id4","id5","id6") 
  names(pre) <- new_names_pre 
  
  # aggragate id variables into one
  pre$id <- paste0(pre$id1, pre$id2, pre$id3, pre$id4, pre$id5, pre$id6)
  pre <- pre [ , -which(names(pre) %in% c("id1","id2","id3","id4","id5","id6"))]
  
  
  # posttest
  post <- read.csv(path2, sep=";",stringsAsFactors=FALSE)
  post <- post [c(-1)]
  
  # rename variables
  new_names_post <- c(paste0("qp",num),"id1","id2","id3","id4","id5","id6")
  names(post) <- new_names_post
  
  # aggragate id variables into one
  post$id <- paste0(post$id1, post$id2, post$id3, post$id4, post$id5, post$id6)
  post <- post [ , -which(names(post) %in% c("id1","id2","id3","id4","id5","id6"))]
  
  #merge
  data <- merge(pre, post, by = "id")
  data[is.na(data)] <- "Z" # mark blanks as "Z" so they will be recorded as wrong
  
  
  # create variable names
  dummy <- paste0("dummyq", num)
  dummypq <- paste0("dummypq", num)
  dummies <- c(dummy, dummypq)
  qvar <- paste0("q", 1:nc)
  qpvar <- paste0("qp", 1:nc)
  trvar  <- paste0("tr", 1:nc)
  
  
  # create data sets
  nr    <- nrow(data)                   # number of observations
  q     <- data[qvar]                   # pretest
  qp    <- data[qpvar]                  # posttest
  score <- data.frame(matrix(nrow = nr, ncol = 2 * nc)) #scoring
  names(score) <- dummies
  trans <- data.frame(matrix(nrow = nr,
                             ncol = nc)) # transitions
  names(trans) <- trvar
  
  
  # score transitions
  for (c in 1:nc) {
    for (r in 1:nr) {
      k <- key[c]
      if (q[r, c] == k & qp[r, c] == k)
        trans[r, c] <- "RR"
      if (q[r, c] == k & qp[r, c] != k)
        trans[r, c] <- "RW"
      if (q[r, c] != k & qp[r, c] == k)
        trans[r, c] <- "WR"
      if (q[r, c] != k & q[r, c] == qp[r, c])
        trans[r, c] <- "WSW"
      if (q[r, c] != k & qp[r, c] != k & qp[r, c] != q[r, c])
        trans[r, c] <- "WDW"
      
      #debugging code
      # cat("c=", c, "r=", r, "\n")
      # cat("k=", k, "q[r, c]=", q[r, c], "qp[r, c]= ", qp[r, c], "\n")
      # cat("trans[r, c]=", trans[r, c], "\n")
      # 
    }
  }
  
  # calculate metrics
  
  # get row sums of RR RW WR WSW and WDW
  sRR  <- rowSums(trans == "RR")
  sRW  <- rowSums(trans == "RW")
  sWR  <- rowSums(trans == "WR")
  sWSW <- rowSums(trans == "WSW")
  sWDW <- rowSums(trans == "WDW")
  
  trans$TotalRR <- sRR
  trans$TotalRW <- sRW
  trans$TotalWR <- sWR
  trans$TotalWSW <- sWSW
  trans$TotalWDW <- sWDW
  
  # calculate statistics
  # not sum percents will be undefined with zero denominator
  trans$Q <- (sRW + sWR + sWDW) / (sRR + sWSW + sRW + sWR + sWDW)
  trans$P <- (sWR) / (sRW + sWR + sWDW)
  trans$C <- (sRW) / (sRW + sWR + sWDW)
  trans$S <- (sWDW) / (sRW + sWR + sWDW)
  trans$K <- (sRR) / (sRR + sRW)
  trans$M <- (sWSW) / (sWDW + sWSW + sWR)
  
  # calculate pre and post score (and round them)
  trans$prescore <- (sRR + sRW) / nc * 100
  trans$roundedprescore <- round(trans$prescore, digits = 0)
  trans$postscore <- (sRR + sWR) / nc * 100
  trans$roundedpostscore <- round(trans$postscore, digits = 0)
  
  # calculate hakes
  trans$hakes <- (trans$postscore - trans$prescore) / (100 - trans$prescore)
  trans$roundedhakes <- round(trans$hakes, digits = 2)
  
  # #set infintie and non-real Hake's gains to appropriate values
  # score$hakes[is.nan(score$hakes)]<-NA
  # score$hakes[score$postscore<score$prescore]<-
  #   (score$postscore[score$postscore<score$prescore]-
  #      score$prescore[score$postscore<score$prescore])/
  #   score$prescore[score$postscore<score$prescore]
  # outputting values
  
  
  # merge results with id variable
  
  final_data <- as.data.frame(cbind(data$"id", trans))
  
  # column sums of transitions
  columns <- ncol(final_data) # get number of columns
  blank_spaces <- columns - (nc + 1) 
  
  sRRcol <- colSums(final_data[,1:(nc+1)] == "RR")
  sRRcol <- c(sRRcol, rep(NA, times = blank_spaces))
  
  sRWcol <- colSums(final_data[,1:(nc+1)] == "RW")
  sRWcol <- c(sRWcol, rep(NA, times = blank_spaces))
  
  sWRcol <- colSums(final_data[,1:(nc+1)] == "WR")
  sWRcol <- c(sWRcol, rep(NA, times = blank_spaces))
  
  sWSWcol <- colSums(final_data[,1:(nc+1)] == "WSW")
  sWSWcol <- c(sWSWcol, rep(NA, times = blank_spaces))
  
  sWDWcol <- colSums(final_data[,1:(nc+1)] == "WDW")
  sWDWcol <- c(sWDWcol, rep(NA, times = blank_spaces))
  
  final_data[nrow(final_data) + 1,] <- sRRcol
  final_data[nrow(final_data) + 1, ] <- sRWcol
  final_data[nrow(final_data) + 1, ] <- sWRcol
  final_data[nrow(final_data) + 1, ] <- sWSWcol
  final_data[nrow(final_data) + 1, ] <- sWDWcol
  
  # column sums of metrics
  final_data[nrow(final_data) + 1,] <- (sRWcol + sWRcol + sWDWcol) / 
    (sRRcol + sWSWcol + sRWcol + sWRcol + sWDWcol)
  final_data[nrow(final_data) + 1,] <- (sWRcol) / (sRWcol + sWRcol + sWDWcol)
  final_data[nrow(final_data) + 1,] <- (sRWcol) / (sRWcol + sWRcol + sWDWcol)
  final_data[nrow(final_data) + 1,] <- (sWDWcol) / (sRWcol + sWRcol + sWDWcol)
  final_data[nrow(final_data) + 1,] <- (sRRcol) / (sRRcol + sRWcol)
  final_data[nrow(final_data) + 1,] <- (sWSWcol) / (sWDWcol + sWSWcol + sWRcol)
  
  # column percents of correct answer 
  final_data[nrow(final_data) + 1,] <- (sRRcol + sRWcol) / nr * 100  # pretest
  final_data[nrow(final_data) + 1,] <- (sRRcol + sWRcol) / nr * 100 # posttest
  
  # label rows
  num2 <- c(1:nrow(trans))
  row_names <- c(paste0("obs ",num2), "Total RR", "Total RW", "Total WR", "Total WSW",
                 "Total WDW", "Q", "P", "C", "S", "K", "M", "Percent Right on Pretest",
                 "Percent Right on Posttest") 
  final_data <- cbind (row_names, final_data)
  
  # pie chart
  RR <- sum(sRR)
  RW <- sum(sRW)
  WR <- sum(sWR)
  WDW <- sum(sWDW)
  WSW <- sum(sWSW)
  tablevalues <- c(RR, RW, WR, WDW, WSW)
  tablenames <- c("RR", "RW", "WR", "WDW", "WSW")
  percent <- (tablevalues / (nrow(data) * nc / 100))
  roundpercent <- round(percent, digits = 2)
  lbls <- paste(tablenames, "(", roundpercent, "%)", sep = "")
  
  # class summary for metrics
  classRR <- percent[1]
  classRW <- percent[2]
  classWR <- percent[3]
  classWDW <- percent[4]
  classWSW <- percent[5]
  
  classQ <- (classRW + classWR + classWDW) / (classRR + classWSW + classRW + classWR + classWDW)
  classP <- (classWR) / (classRW + classWR + classWDW)
  classC <- (classRW) / (classRW + classWR + classWDW)
  classS <- (classWDW) / (classRW + classWR + classWDW)
  classK <- (classRR) / (classRR + classRW)
  classM <- (classWSW) / (classWDW + classWSW + classWR)
  
  # display text
  Qtext <- paste("Q:", round(classQ, 2))
  Ptext <- paste("P:", round(classP, 2))
  Ctext <- paste("C:", round(classC, 2))
  Stext <- paste("S:", round(classS, 2))
  Ktext <- paste("K:", round(classK, 2))
  Mtext <- paste("M:", round(classM, 2))
  
  cat("Class Metrics\n", Qtext, "\n", Ptext, "\n", Ctext, "\n", Stext, "\n", Ktext, "\n",
      Mtext)
  
  piechart <- pie(tablevalues, labels = lbls)
  
  return(final_data)
} 