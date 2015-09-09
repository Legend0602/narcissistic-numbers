##Narc Numbers

##In recreational number theory, a narcissistic number 
##(also known as a pluperfect digital invariant (PPDI),
##an Armstrong number (after Michael F. Armstrong) or 
##a plus perfect number) is a number that is the sum of its
##own digits each raised to the power of the number of digits.n

narcNum <- function(digit){

  ##set starting and end number for narcNum test

  pow <- digit - 1
  startNum <- 10 ^(pow)
  endNum <- 10^(digit) - 1
  testVector<- c(startNum:endNum)
  catPile <- c()  ## will hold all narc numbers
  
  for (i in testVector) {
    kittenPile <- c()
    powA <- pow
    place <- 10 ^(powA)
    num1 <- i
    
    while(place >= 1){
      ans <- floor(num1/place)
      ans1 <- ans^digit
      kittenPile <- c(kittenPile, ans1)
      num1 <- (num1 - (ans*place))
      place<- place/10
     }
  
    if(sum(kittenPile) == i){
      catPile<- c(catPile, i)
    }
  }
     
  print(catPile)

}
