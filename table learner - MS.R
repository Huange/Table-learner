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

n_MS <- 5
MS_states <- c('5','4','3','2','1')
n_Age <- 4
Age_states <- c('<80','70-79','60-69','<60')
n_Diabetes <- 3
Diabetes_states <- c('with CKD','without CKD','No')
n_BMI <- 4
BMI_states <- c('Obese','Overweight','Normal','Underweight')
n_Gender <- 2
Gender_states <- c('Male','Female')
table_MS <- matrix(0, nrow = n_MS, ncol = n_Age*n_Diabetes*n_BMI*n_Gender)

# (2)Diabetes, (8)Age, (9)BMI, (10)Gender, (13)Metabolic Syndrome
data <- subdata_total
n_data <- nrow(data)
for (t in 1:n_data) {
  if (isTRUE(data[t,8]=='80<')) {
    if (isTRUE(data[t,2]=='with CKD')) {
      if (isTRUE(data[t,9]=='Obese')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 1
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 2
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Overweight')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 3
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 4
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Normal')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 5
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 6
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Underweight')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 7
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 8
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
    }
    if (isTRUE(data[t,2]=='without CKD')) {
      if (isTRUE(data[t,9]=='Obese')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 9
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 10
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Overweight')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 11
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 12
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Normal')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 13
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 14
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Underweight')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 15
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 16
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
    }
    if (isTRUE(data[t,2]=='No')) {
      if (isTRUE(data[t,9]=='Obese')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 17
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 18
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Overweight')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 19
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 20
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Normal')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 21
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 22
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Underweight')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 23
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 24
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
    }
  }
  if (isTRUE(data[t,8]=='70-79')) {
    if (isTRUE(data[t,2]=='with CKD')) {
      if (isTRUE(data[t,9]=='Obese')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 25
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 26
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Overweight')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 27
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 28
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Normal')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 29
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 30
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Underweight')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 31
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 32
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
    }
    if (isTRUE(data[t,2]=='without CKD')) {
      if (isTRUE(data[t,9]=='Obese')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 33
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 34
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Overweight')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 35
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 36
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Normal')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 37
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 38
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Underweight')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 39
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 40
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
    }
    if (isTRUE(data[t,2]=='No')) {
      if (isTRUE(data[t,9]=='Obese')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 41
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 42
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Overweight')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 43
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 44
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Normal')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 45
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 46
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Underweight')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 47
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 48
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
    }
  }
  if (isTRUE(data[t,8]=='60-69')) {
    if (isTRUE(data[t,2]=='with CKD')) {
      if (isTRUE(data[t,9]=='Obese')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 49
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 50
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Overweight')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 51
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 52
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Normal')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 53
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 54
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Underweight')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 55
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 56
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
    }
    if (isTRUE(data[t,2]=='without CKD')) {
      if (isTRUE(data[t,9]=='Obese')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 57
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 58
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Overweight')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 59
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 60
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Normal')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 61
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 62
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Underweight')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 63
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 64
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
    }
    if (isTRUE(data[t,2]=='No')) {
      if (isTRUE(data[t,9]=='Obese')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 65
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 66
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Overweight')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 67
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 68
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Normal')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 69
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 70
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Underweight')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 71
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 72
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
    }
  }
  if (isTRUE(data[t,8]=='<60')) {
    if (isTRUE(data[t,2]=='with CKD')) {
      if (isTRUE(data[t,9]=='Obese')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 73
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 74
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Overweight')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 75
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 76
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Normal')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 77
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 78
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Underweight')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 79
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 80
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
    }
    if (isTRUE(data[t,2]=='without CKD')) {
      if (isTRUE(data[t,9]=='Obese')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 81
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 82
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Overweight')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 83
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 84
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Normal')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 85
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 86
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Underweight')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 87
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 88
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
    }
    if (isTRUE(data[t,2]=='No')) {
      if (isTRUE(data[t,9]=='Obese')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 89
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 90
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Overweight')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 91
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 92
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Normal')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 93
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 94
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
      if (isTRUE(data[t,9]=='Underweight')) {
        if (isTRUE(data[t,10]=='Male')) {
          j <- 95
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
        if (isTRUE(data[t,10]=='Female')) {
          j <- 96
          if (isTRUE(data[t,13]=='5')) {
            table_MS[1,j] <- table_MS[1,j] + 1
          }
          if (isTRUE(data[t,13]=='4')) {
            table_MS[2,j] <- table_MS[2,j] + 1
          }
          if (isTRUE(data[t,13]=='3')) {
            table_MS[3,j] <- table_MS[3,j] + 1
          }
          if (isTRUE(data[t,13]=='2')) {
            table_MS[4,j] <- table_MS[4,j] + 1
          }
          if (isTRUE(data[t,13]=='1')) {
            table_MS[5,j] <- table_MS[5,j] + 1
          }
        }
      }
    }
  }
}

# nrow(subdata_total)
# sum(table_MS)
# 184180
write.csv(table_MS, file = "table_MS_count.csv", row.names = FALSE)

for (i in 1:ncol(table_MS)) {
  table_MS[,i] <- table_MS[,i]/sum(table_MS[,i])
}

write.csv(table_MS, file = "table_MS_freq.csv", row.names = FALSE)
