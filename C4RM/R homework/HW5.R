##################################### Q1: qMatMult1

MyMatMult = function(vec,mat){
  
  nRowMat = dim(mat)[1]
  nColMat = dim(mat)[2]
  nRowVec = dim(vec)[1]
  nColVec = dim(vec)[2]
  
  ans = matrix(0,nRowVec,nColMat)
  
  for(i in 1:nColVec){
    ans[,i] = array(colSums(t(vec)*mat[,i]))
  }
  
  return(ans)
}

# Unit test

vec = matrix(data = 1:3,nrow = 1,ncol = 3)
mat = matrix(data = 1:9,nrow = 3,ncol = 3,byrow = T)
ans = vector(mode = "numeric",length = 3)


x = MyMatMult(vec,mat)
x[1] == 30 & x[2] == 36 & x[3] == 42

functionText = capture.output(MyMatMult)
if(length(grep("for",functionText))!=1){
  cat("Only 1 'for' loop allowed!")
}

functionText = capture.output(MyMatMult)
if(length(grep("%*%",functionText))>0){
  cat("No Cheating :)")
}

##################################### Q2: QMatMult2

MatMult2 = function(vec,mat){
  
  nRowMat = dim(mat)[1]
  nColMat = dim(mat)[2]
  nRowVec = dim(vec)[1]
  nColVec = dim(vec)[2]
  
  ans = matrix(0,nRowVec,nColMat)
  
  if (nRowVec == 1 & nColMat == 1){
    ans = sum(vec*mat[,1])
  }else{
    for(i in 1:nColVec){
      ans[,i] = array(colSums(t(vec)*mat[,i]))
    }
  }
  return(ans)
}

# Unit test

vec = matrix(data = 1:3,nrow = 1,ncol = 3)
mat = matrix(data = c(1,4,7),nrow = 3,ncol = 1)

x = MatMult2(vec,mat)
x[1] == 30 

functionText = capture.output(MatMult2)
if(length(grep("for",functionText))!=1){
  cat("Only 1 'for' loop allowed!")
}

functionText = capture.output(MatMult2)
if(length(grep("%*%",functionText))>0){
  cat("No Cheating :)")
}

##################################### Q3: TMat1

TMAT1 = function(vec1,vec2){
  
  unio = union(vec1, vec2)
  uniq = sort(unique(unio))
  mrowcol = length(uniq)
  
  ans = matrix(0,mrowcol,mrowcol)
  
  for (i in uniq){
    if(sum(vec1 == i)>0){
      vec2test = vec2[which(vec1 == i)]
      vec2tuniq = sort(unique(vec2test))
      iloc = which(uniq == i)
      for(j in vec2tuniq){
        jloc = which(uniq == j)
        ans[iloc,jloc] = sum(vec2test == j)
      }
      ans[iloc,] = ans[iloc,]/sum(vec1 == i)
    }
  }
  mat = ans
  return(mat)
}


rLast = rep(c('A','B','C'),c(3,4,3))
rNow  = rep(c('A','B','C'),c(5,0,5))

# Unit Tests
out = TMAT1(rLast,rNow)
answer = c(1,.5,0,0,0,0,0,.5,1)
dim(answer) = c(3,3)
all.equal(out,answer)

##################################### Q4: TMat2

TMatMult = function(vec,mat){
  
  nRowMat = dim(mat)[1]
  nColMat = dim(mat)[2]
  nRowVec = dim(vec)[1]
  nColVec = dim(vec)[2]
  
  ans = matrix(0,nRowVec,nColMat)
  
  for(i in 1:nColVec){
    ans[,i] = array(colSums(t(vec)*mat[,i]))
  }
  
  return(ans)
}

# Unit tests

vec = matrix(data = c(20,30,10),nrow = 1,ncol = 3)
mat = c(1,.5,0,0,0,0,0,.5,1)
dim(mat) = c(3,3)

x = TMatMult(vec,mat)
x[1] == 35 & x[2] == 0 & x[3] == 25

functionText = capture.output(TMatMult)
if(length(grep("for",functionText))!=1){
  cat("Only 1 'for' loop allowed!")
}

##################################### Q5: qBondPrice

getBondPrice = function(y, face, couponRate, m, ppy=1){
  cf = (face * couponRate) / ppy
  t = m * ppy
  pmtc = (1/( 1 + (y / ppy)))^(1:t)
  pcfc = array(cf, t)
  pcfc[t] = pcfc[t] + face
  pcfc = pcfc*pmtc
  bondPrice = sum(pcfc)
  return (bondPrice)
}


# Unit tests

y = 0.03
face = 2000000
couponRate = 0.04
m = 10
ppy = 2

x = getBondPrice(y, face, couponRate, m,  1)
round(x) == 2170604

x = getBondPrice(y, face, couponRate, m,  2)
round(x) == 2171686

functionText = capture.output(getBondPrice)
if(length(grep("for",functionText))){
  cat("No 'for' loops allowed!")
}

##################################### Q6£ºqBondDuration

getBondDuration = function(y, face, couponRate, m, ppy = 1){
  
  cf = (face * couponRate) / ppy
  t = m * ppy
  pmtc = (1/( 1 + (y / ppy)))^(1:t)
  pcfc = array(cf, t)
  pcfc[t] = pcfc[t] + face
  pcfc = pcfc*pmtc
  tc = array(1:t)
  wt = sum(pcfc*tc)
  bp = sum(pcfc)
  
  duration = wt/bp

  return (duration)
}

# Unit tests

y = 0.03
face = 2000000
couponRate = 0.04
m = 10

x = getBondDuration(y, face, couponRate, m,  1)
round(x,2) == 8.51

functionText = capture.output(getBondDuration)
if(length(grep("for",functionText))){
  cat("No 'for' loops allowed!")
}

##################################### Q7£ºqBondPriceFromCurve

getBondPriceYC = function(y, face, couponRate, m){
  
  pmt = array(y, m)
  pmt = (1+pmt)^(-(1:m))
  bondprice = (sum(pmt)*couponRate+pmt[m])*face
  return (bondprice)
}

# Unit tests
library(readxl)
YC <- read_excel("D:/Study/C4RM/HW5/C4RM_Class5.xlsx",
                 sheet = "qBondPriceFromCurve", range = "G3:G12",
                 col_names = FALSE)

YC = as.matrix(YC)
y = .03
face = 2000000
couponRate = 0.04
m = 10
ppy = 2

x1 = getBondPriceYC(y, face, couponRate, m)
round(x1) == 2170604

x2 = getBondPriceYC(YC, face, couponRate, m)
round(x2) ==  1267138

functionText = capture.output(getBondPriceYC)
if(
  length(grep("if",functionText))==0 &
  round(x1) == 2170604 &
  round(x2) ==  1267138
){
  cat("Congraulations!\n")
  cat("You answered without using 'if'.\n")
  cat("You get an extra point in this assignment.")
}

##################################### Q8: qCondition

FizzBuzz = function(start,finish){
  n = finish-start+1
  v = vector(mode = "character",length = n)
  x = c(start:finish)
  ind15 = x %% 15 == 0
  ind3 = x %% 3 == 0
  ind5 = x %% 5 == 0
  x[ind3] = "fizz"
  x[ind5] = "buzz"
  x[ind15] = "fizzbuzz"
  v[1:n] = x
  return(v)
}

# Unit test

x = FizzBuzz(40,45)
x[1] == "buzz" & x[2] == "41" & x[6] == "fizzbuzz"

##################################### Q9: GetReturns

# Create a function called getReturns that returns a vector of returns

# You only need to install once

library(quantmod)

getStockData = function(symbol){
  mydata = quantmod::getSymbols(Symbols = symbol,auto.assign = F)
  prices = mydata[,6]
  return(prices)
}

prices = getStockData('gs')
class(prices)
pricevec = as.matrix(prices)

getReturns = function(pricemat){
  n = length(pricemat)
  ratiovec = pricemat[2:n]/pricemat[1:(n-1)]
  returns =  ratiovec - 1
  return(returns)
}
