set.seed(100)
install.packages("writexl")
library(readxl)
library("writexl")
setwd("D:/Matlab codes/10k/combine 2 data sets")
install.packages("BurStMisc")
library("BurStMisc")

# subdata_1 <- read.csv(file = 'subdata_1_17_complete.csv')
# subdata_2 <- read.csv(file = 'subdata_2_17_complete.csv')
subdata_total <- read.csv(file = 'subdata_total_17_complete.csv')

n_Immuno <- 3
Immuno_states <- c('3','2','1')
n_Diabetes <- 3
Diabetes_states <- c('with CKD','without CKD','No')
n_Cancer <- 2
Cancer_states <- c('Yes','No')
n_ODB <- 3
ODB_states <- c('>1','1','0')
table_Immuno <- matrix(0, nrow = n_Immuno, ncol = n_Diabetes*n_Cancer*n_ODB)

# (2)Diabetes, (5)Cancer, (6)Other Disease Burden, (12)Immunosuppression
data <- subdata_total
n_data <- nrow(data)
for (t in 1:n_data) {
  if (isTRUE(data[t,2]=='with CKD')) {
    if (isTRUE(data[t,5]=='Yes')) {
      if (isTRUE(data[t,6]=='>1')) {
        if (isTRUE(data[t,12]=='3')) {
          table_Immuno[1,1] <- table_Immuno[1,1] + 1
        }
        if (isTRUE(data[t,12]=='2')) {
          table_Immuno[2,1] <- table_Immuno[2,1] + 1
        }
        if (isTRUE(data[t,12]=='1')) {
          table_Immuno[3,1] <- table_Immuno[3,1] + 1
        }
      }
      if (isTRUE(data[t,6]=='1')) {
        if (isTRUE(data[t,12]=='3')) {
          table_Immuno[1,2] <- table_Immuno[1,2] + 1
        }
        if (isTRUE(data[t,12]=='2')) {
          table_Immuno[2,2] <- table_Immuno[2,2] + 1
        }
        if (isTRUE(data[t,12]=='1')) {
          table_Immuno[3,2] <- table_Immuno[3,2] + 1
        }
      }
      if (isTRUE(data[t,6]=='0')) {
        if (isTRUE(data[t,12]=='3')) {
          table_Immuno[1,3] <- table_Immuno[1,3] + 1
        }
        if (isTRUE(data[t,12]=='2')) {
          table_Immuno[2,3] <- table_Immuno[2,3] + 1
        }
        if (isTRUE(data[t,12]=='1')) {
          table_Immuno[3,3] <- table_Immuno[3,3] + 1
        }
      }
    }
    if (isTRUE(data[t,5]=='No')) {
      if (isTRUE(data[t,6]=='>1')) {
        if (isTRUE(data[t,12]=='3')) {
          table_Immuno[1,4] <- table_Immuno[1,4] + 1
        }
        if (isTRUE(data[t,12]=='2')) {
          table_Immuno[2,4] <- table_Immuno[2,4] + 1
        }
        if (isTRUE(data[t,12]=='1')) {
          table_Immuno[3,4] <- table_Immuno[3,4] + 1
        }
      }
      if (isTRUE(data[t,6]=='1')) {
        if (isTRUE(data[t,12]=='3')) {
          table_Immuno[1,5] <- table_Immuno[1,5] + 1
        }
        if (isTRUE(data[t,12]=='2')) {
          table_Immuno[2,5] <- table_Immuno[2,5] + 1
        }
        if (isTRUE(data[t,12]=='1')) {
          table_Immuno[3,5] <- table_Immuno[3,5] + 1
        }
      }
      if (isTRUE(data[t,6]=='0')) {
        if (isTRUE(data[t,12]=='3')) {
          table_Immuno[1,6] <- table_Immuno[1,6] + 1
        }
        if (isTRUE(data[t,12]=='2')) {
          table_Immuno[2,6] <- table_Immuno[2,6] + 1
        }
        if (isTRUE(data[t,12]=='1')) {
          table_Immuno[3,6] <- table_Immuno[3,6] + 1
        }
      }
    }
  }
  if (isTRUE(data[t,2]=='without CKD')) {
    if (isTRUE(data[t,5]=='Yes')) {
      if (isTRUE(data[t,6]=='>1')) {
        if (isTRUE(data[t,12]=='3')) {
          table_Immuno[1,7] <- table_Immuno[1,7] + 1
        }
        if (isTRUE(data[t,12]=='2')) {
          table_Immuno[2,7] <- table_Immuno[2,7] + 1
        }
        if (isTRUE(data[t,12]=='1')) {
          table_Immuno[3,7] <- table_Immuno[3,7] + 1
        }
      }
      if (isTRUE(data[t,6]=='1')) {
        if (isTRUE(data[t,12]=='3')) {
          table_Immuno[1,8] <- table_Immuno[1,8] + 1
        }
        if (isTRUE(data[t,12]=='2')) {
          table_Immuno[2,8] <- table_Immuno[2,8] + 1
        }
        if (isTRUE(data[t,12]=='1')) {
          table_Immuno[3,8] <- table_Immuno[3,8] + 1
        }
      }
      if (isTRUE(data[t,6]=='0')) {
        if (isTRUE(data[t,12]=='3')) {
          table_Immuno[1,9] <- table_Immuno[1,9] + 1
        }
        if (isTRUE(data[t,12]=='2')) {
          table_Immuno[2,9] <- table_Immuno[2,9] + 1
        }
        if (isTRUE(data[t,12]=='1')) {
          table_Immuno[3,9] <- table_Immuno[3,9] + 1
        }
      }
    }
    if (isTRUE(data[t,5]=='No')) {
      if (isTRUE(data[t,6]=='>1')) {
        if (isTRUE(data[t,12]=='3')) {
          table_Immuno[1,10] <- table_Immuno[1,10] + 1
        }
        if (isTRUE(data[t,12]=='2')) {
          table_Immuno[2,10] <- table_Immuno[2,10] + 1
        }
        if (isTRUE(data[t,12]=='1')) {
          table_Immuno[3,10] <- table_Immuno[3,10] + 1
        }
      }
      if (isTRUE(data[t,6]=='1')) {
        if (isTRUE(data[t,12]=='3')) {
          table_Immuno[1,11] <- table_Immuno[1,11] + 1
        }
        if (isTRUE(data[t,12]=='2')) {
          table_Immuno[2,11] <- table_Immuno[2,11] + 1
        }
        if (isTRUE(data[t,12]=='1')) {
          table_Immuno[3,11] <- table_Immuno[3,11] + 1
        }
      }
      if (isTRUE(data[t,6]=='0')) {
        if (isTRUE(data[t,12]=='3')) {
          table_Immuno[1,12] <- table_Immuno[1,12] + 1
        }
        if (isTRUE(data[t,12]=='2')) {
          table_Immuno[2,12] <- table_Immuno[2,12] + 1
        }
        if (isTRUE(data[t,12]=='1')) {
          table_Immuno[3,12] <- table_Immuno[3,12] + 1
        }
      }
    }
  }
  if (isTRUE(data[t,2]=='No')) {
    if (isTRUE(data[t,5]=='Yes')) {
      if (isTRUE(data[t,6]=='>1')) {
        if (isTRUE(data[t,12]=='3')) {
          table_Immuno[1,13] <- table_Immuno[1,13] + 1
        }
        if (isTRUE(data[t,12]=='2')) {
          table_Immuno[2,13] <- table_Immuno[2,13] + 1
        }
        if (isTRUE(data[t,12]=='1')) {
          table_Immuno[3,13] <- table_Immuno[3,13] + 1
        }
      }
      if (isTRUE(data[t,6]=='1')) {
        if (isTRUE(data[t,12]=='3')) {
          table_Immuno[1,14] <- table_Immuno[1,14] + 1
        }
        if (isTRUE(data[t,12]=='2')) {
          table_Immuno[2,14] <- table_Immuno[2,14] + 1
        }
        if (isTRUE(data[t,12]=='1')) {
          table_Immuno[3,14] <- table_Immuno[3,14] + 1
        }
      }
      if (isTRUE(data[t,6]=='0')) {
        if (isTRUE(data[t,12]=='3')) {
          table_Immuno[1,15] <- table_Immuno[1,15] + 1
        }
        if (isTRUE(data[t,12]=='2')) {
          table_Immuno[2,15] <- table_Immuno[2,15] + 1
        }
        if (isTRUE(data[t,12]=='1')) {
          table_Immuno[3,15] <- table_Immuno[3,15] + 1
        }
      }
    }
    if (isTRUE(data[t,5]=='No')) {
      if (isTRUE(data[t,6]=='>1')) {
        if (isTRUE(data[t,12]=='3')) {
          table_Immuno[1,16] <- table_Immuno[1,16] + 1
        }
        if (isTRUE(data[t,12]=='2')) {
          table_Immuno[2,16] <- table_Immuno[2,16] + 1
        }
        if (isTRUE(data[t,12]=='1')) {
          table_Immuno[3,16] <- table_Immuno[3,16] + 1
        }
      }
      if (isTRUE(data[t,6]=='1')) {
        if (isTRUE(data[t,12]=='3')) {
          table_Immuno[1,17] <- table_Immuno[1,17] + 1
        }
        if (isTRUE(data[t,12]=='2')) {
          table_Immuno[2,17] <- table_Immuno[2,17] + 1
        }
        if (isTRUE(data[t,12]=='1')) {
          table_Immuno[3,17] <- table_Immuno[3,17] + 1
        }
      }
      if (isTRUE(data[t,6]=='0')) {
        if (isTRUE(data[t,12]=='3')) {
          table_Immuno[1,18] <- table_Immuno[1,18] + 1
        }
        if (isTRUE(data[t,12]=='2')) {
          table_Immuno[2,18] <- table_Immuno[2,18] + 1
        }
        if (isTRUE(data[t,12]=='1')) {
          table_Immuno[3,18] <- table_Immuno[3,18] + 1
        }
      }
    }
  }
}

# nrow(subdata_total)
# sum(table_Immuno)
# 184180
write.csv(table_Immuno, file = "table_Immuno_count.csv", row.names = FALSE)

for (i in 1:ncol(table_Immuno)) {
  table_Immuno[,i] <- table_Immuno[,i]/sum(table_Immuno[,i])
}

write.csv(table_Immuno, file = "table_Immuno_freq.csv", row.names = FALSE)
