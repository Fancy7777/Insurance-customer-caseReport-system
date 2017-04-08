transf <- function(table_use) {
  ##run throught every column
  for (i in (1:length(table_use))) {
    ## run through every column which is not numeric type except "claimno_claim"
    if (class(table_use[,i]) == "factor" &&
        names(table_use)[i] != "claimno_claim") {
      ##run through every element in this column
      for (j in (1:length(table(table_use[,i])))) {
        pos <-
          which(levels(table_use[,i]) == names(table(table_use[,i]))[j])
        value <-
          take_freq(table_use[,i],names(table(table_use[,i]))[j]) / length(table_use[,i])
        levels(temp[,i])[pos] <<- value
      }
    }
  }
  return(table_use)
}


##take the frequency out from one column
take_freq <- function(column, elem) {
  temp <- as.data.frame(table(column))
  ##loop the table() and return the corresponding the frequence
  for (i in (1:nrow(temp))) {
    if (elem == temp[i,1]) {
      return(temp[i,2])
    }
  }
}

##换成factor形式
transfactor <- function(table_use){
  for (i in (1:length(table_use))) {
    if(is.character(table_use[,i])){
      temp[,i]<<-as.factor(table_use[,i])
    }
  }
}

transnumeric <- function(table_use){
  for (i in (1:length(table_use))) {
    if(is.factor(table_use[,i]) && names(table_use)[i] != "claimno_claim"){
      temp[,i]<<-as.numeric(as.character(table_use[,i]))
    }
  }
}


##转数据框
correlation<-as.numeric(correlation)
##新建一个matrix放数据，具体情况具体建
temp2<-matrix(nrow=68, ncol = 136)
##列前加列，然后
infras_transf <- function(table_use) {
  for (i in (1:(ncol(table_use) * 2))) {
    if (i %% 2 == 0) {
      temp2[,i] <<- table_use[,i / 2]
    }
    else if(i %% 2 == 1) {
      temp2[,i] <<- rownames(table_use)
    }
  }
}

temp3<-as.data.frame(temp3)
trans_numeric_ou <- function(table_use){
  for (i in (1:length(table_use))) {
    if(i %% 2 ==0){
      temp3[,i]<<-as.numeric(as.character(table_use[,i]))
    }
  }
}




##两列两列的排序
sort_two <- function(table_use){
  for(i in (1:length(table_use))){
    if(i %% 2 ==0){
      temp3[,i] <<- table_use[order(abs(table_use[,(i)]),decreasing = TRUE), 
                              colnames(table_use)[i]]
      temp3[,i-1] <<- table_use[order(abs(table_use[,(i)]),decreasing = TRUE), 
                              colnames(table_use)[i-1]]
      
    }
  }
}

y<-matrix(nrow = 1)
##挑出factor的名字
pick_names <- function(table_use) {
  j <- 1
  for (i in (1:length(table_use))) {
      y[j] <<- names(table_use)[i]
      j = j + 1
    
  }
}

##改最后排序的名字
change_final_name<-function(table_use){
  for(i in (1:length(table_use))){
    if(i %% 2 ==0){
      names(temp5)[i] <<- y[i/2]
    }

  }
}
