# Q1: Bond Price

getBondPrice = function(y, face, couponRate, m, ppy=1){
  cf = (face * couponRate) / ppy
  pcf = 0
  t = m * ppy
  pmt = 1/( 1 + (y / ppy))
  for (i in 1:t) {
    pcf = pcf + cf*(pmt**i)
  }
  pmcf = pcf + face*(pmt**t)
  return(pmcf) 
}

# Unit test

y = 0.03
face = 2000000
couponRate = 0.04
m = 10
ppy = 2

x = getBondPrice(y, face, couponRate, m,  1)
round(x) == 2170604

x = getBondPrice(y, face, couponRate, m,  2)
round(x) == 2171686

# Q2: Bond Duration

# This function could also change the ppy variable
getBondDuration = function(y, face, couponRate, m, ppy = 1){
  cf = (face * couponRate) / ppy
  t = m * ppy
  pmt = 1/( 1 + (y / ppy))
  sumw = 0
  pcf = 0
  for (i in 1:t) {
    if (i == t) {
      sumw = sumw + i * (face + cf) * (pmt**i)
      pcf = pcf + (cf + face) * (pmt**i)
    } else {
      sumw = sumw + i * cf * (pmt**i)
      pcf = pcf + cf * (pmt**i)
    }
  }
  w = sumw / pcf
  return(w) 
}

# Unit test

y = 0.03
face = 2000000
couponRate = 0.04
m = 10

x = getBondDuration(y, face, couponRate, m, 1)
round(x,2) == 8.51

# Q3: Bond Effective Sensitivity

# This function could also change the ppy variable
getBPV = function(y, face, couponRate, m,ppy = 1){
  y1 = y / ppy
  y2 = (y + 0.0001) / ppy
  cr = couponRate / ppy
  t = ppy * m
  pmt1 = 1/(1 + y1)
  pmt2 = 1/(1 + y2)
  pv1 = 0
  pv2 = 0
  for (i in 1:t) {
    if (i == t) {
      pv1 = pv1 + pmt1 ** i
      pv2 = pv2 + pmt2 ** i
      sum1 = (pv1 * cr + pmt1 ** t) * face
      sum2 = (pv2 * cr + pmt2 ** t) * face
    } else {
      pv1 = pv1 + pmt1 ** i
      pv2 = pv2 + pmt2 ** i
    }
  }
  sen = sum2 - sum1
  return(sen) 
}

# Unit test

y = 0.03
face = 2000000
couponRate = 0.04
m = 10
ppy = 1

x = getBPV(y, face, couponRate, m,  ppy)
round(x) == -1792

# Q4: Arrays 1

# Could use this function for two non-single-row matrix
MyMatMult = function(vec,mat){
  arow = nrow(vec)
  acol = ncol(mat)
  ans = matrix(nrow = arow, ncol = acol)
  
  for (i in 1:arow)
  {
    for(j in 1:acol)
    {
      vec1 <- vec[i,]
      vec2 <- mat[,j]
      
      mult1 <- vec1 * vec2
      
      ans[i,j] <- sum(mult1)
    }
  }
  
  # for (c in 1:acol){
  #   for (r in 1:arow){
  #     ans[r, c] = vec[r,]%*%mat[,c]
  #   }
  # }
  return(ans)
}

# Unit test

vec = matrix(data = 1:3,nrow = 1,ncol = 3)
mat = matrix(data = 1:9,nrow = 3,ncol = 3,byrow = T)

x = MyMatMult(vec,mat)
x[1] == 30 & x[2] == 36 & x[3] == 42

# Q5: Arrays 2

library(readxl)
d <- read_excel("D:/Study/C4RM/hw4/C4RM_Class4.xlsx", 
                sheet = "qArrays2", range = "C38:Z48", 
                col_names = FALSE)

d = as.matrix(d)

makeObj = function(d){
  x = array(data = 0,dim = c(11,2,3,4))
  for (i_person in 1:4){
    for (i_trip in 1:3){
      for (i_step in 1:11){
        for (i_latlong in 1:2){
          x[i_step, i_latlong, i_trip, i_person] = d[i_step, i_latlong + (i_person - 1) * 2+ (i_trip - 1) * 8]
        }
      }
    }
  }
  return(x)
}

# Unit test:

obj = makeObj(d)
round(obj[2,2,3,4],4) == .4863

# Q6: FizzBuzz

FizzBuzz = function(start,finish){
  n = finish-start+1
  v = vector(mode = "character",length = n)
  for (i in 1:n){
    tester = start - 1 + i
    if (tester %% 15 == 0 ){
      v[i] = "fizzbuzz"
    } else if (tester %% 3 == 0){
      v[i] = "fizz"
    } else if (tester %% 5 == 0){
      v[i] = "buzz"
    } else {
      v[i] = tester
    }
  }
  return(v)
}

# Unit test

x = FizzBuzz(40,45)
x[1] == "buzz" & x[2] == "41" & x[6] == "fizzbuzz"

# Q7: Get Stock 1

library(quantmod)
x = getSymbols(Symbols = 'AAPL', auto.assign = F)

getStockData = function (stock){
  x = getSymbols(Symbols = stock, auto.assign = F)
  return(x) 
}

# Q8: Get Stock 2

# Create a function to download and save the stock data as a csv file
getStockData = function (stock){
  x = getSymbols(Symbols = stock, auto.assign = F)
  return(x) 
}

getStockData2CSV = function (MYSTOCKNAME,MYFILENAME){
  x = getStockData(MYSTOCKNAME)
  write.csv(x, MYFILENAME)
  return(x)
}

getStockData2CSV('AAPL', 'D:/Study/C4RM/hw4/applestock.csv')



