print('Hello R')

# install.packages('rtools')
install.packages('httr')
install.packages('jsonlite')

library(httr)
library(jsonlite)

endpoint<- "https://api.openweathermap.org/data/2.5/weather?q=Warszawa&appid=1765994b51ed366c506d5dc0d0b07b77&units=metric"
class(endpoint)
getWeather <- GET(endpoint)
Weathertxt <- content(getWeather, "text")
Weathertxt

?fromJSON
dataFormJSON <- fromJSON(Weathertxt)
dfFromJSON <- as.data.frame(dataFormJSON)

dataFormJSON2 <- fromJSON(endpoint)
dfFromJSON2 <- as.data.frame(dataFormJSON2)
View

# a=1 dla argumentow funcji
# l<-100 przypisanie wartosci

f <- function(a=1){
  print(a)
}

x <- 100
class(x)

is.numeric(x)
is.vector(x)


### sekwencja

?seq

x[1]
x1 <- 3:5
x1
x2<-seq(1:100)
x2
x2<-seq(20,10, by=-1)
x3<-seq(11,20)

### wektor

?vector

vm<-c(2,4)
vm
vm <-c(2,3,-6,0)

# *+_/

x2*x3
x2+x3
x2-x3
x2/x3

# typy


logiczny <- as.logical(vm)
logiczny
as.integer(logiczny)
class(vm)
znakowy<-c("1","2","3","4")

c(znakowy,vm) 
# liczbowy i logiczny daje numeryczny
class(c(logiczny,vm)) # numeryczny
class(c(znakowy,vm))  # znakowy

# The atomic modes are "logical", "integer", "numeric" (synonym "double"), "complex", "character" and "raw".

### lista

lista <- list(c("a","b","c"),c(1,2,3),c(TRUE,FALSE))
lista        
lista[[2]][3]
wekzlis<-lista[[2]]
wekzlis[1:2]
wektor_flaga <- vm>0
vm[wektor_flaga]
vm[vm>0]
lis_flaga <- lista[[2]]>0
lista[[2]][lista[[2]]!=2]

### macierz

# wektor jako macierz

M <- matrix(data=NA, nrow=1, ncol=1, byrow=FALSE, dimnames=NULL)
M <- matrix()
wek <- 1:10
M <- as.matrix(wek)
A <- matrix(1:10, ncol=5)
A
w<-11:20

?rbind
?cbind

rbind(wek,w) # zlacza wierszami
cbind(wek,w) # zlacza kolumnami

?t # transpozycja
A
t(A)

# mnozenie macierzy
?'%%'
B <- matrix(11:20, ncol=5)
A%%t(A)
A%%B
?'/' # oraz
?'%%' # oraz 
?'%/%' # modulo

# typ factor
plec <- c("k","m","k","m")
plecFactor <- as.factor(plec)
class(plecFactor)
unclass(plecFactor)
as.numeric(plecFactor)
?factor
plecFactor2 <- factor(plec, levels = c("m","k"))


### ramka danych

id <- c(1,2,3)
imie <- c("jan","kamila","arek")
plec <- c("m","k","m")

?data.frame
ram <- data.frame(numer=id,imie,plec)
ram
class(ram)
summary(ram)

ram[2]
ram[,2]
ram[2,]
ram[3,1]
ram['imie']

View(ram)
ram$numer

dflrow <-data.frame(numer=4, imie='michal', plec='m')
ram <- rbind(ram,dflrow)
length(ram$numer)
length(ram)

### for

lirow <-nrow(ram)

for (i in 1:lirow){
  if (i>2)
    print(ram[i,])
}

for (i in 1:lirow){
  if (ram[i,"imie"]!="arek")
    print(ram[i,])
}

for (i in 1:lirow){
  if (ram[i,"imie"]!="arek"&ram[i,"imie"]!="michal")
    print(ram[i,])
}

# != == < > & |
?'&'

### while :break, next
### if :if,else if, else

  x <- 100
l <- ""
i <- 1
while (i<10) {
  if(i==5)
    l=i
  i=i+1
}

i <-1
while (i<lirow) {
  print(i)
  i=i+1
}

i <-1
while (i<lirow) {
  print(i)
}

### funkcja

podajImie <- function(){
  print("Witaj ")
}
podajImie()

podaj <- function(a="user"){
  print(paste("Witaj ",a))
}
podaj("Ola")

podaj1 <- function(){
  line<-readline(prompt="Podaj swoje imie:")
  print(line)
}
podaj1()


podaj2 <- function(sep=","){
  line<-readline(prompt=paste0("Podaj swoje imie i imie psa oddzielone ",sep,","))
  podz <- strsplit(line,sep)
  print(paste0("Ty", podz[[1]] [1]))
  print(paste0("pies", podz[[1]] [2]))
}
podaj2()

### read csv

?read.csv
?read.table

read.csv("dane.csv")


mean(dane)
median(dane)
max(dane)
min(dane)
summary(dane)

ommit # ponmija brakujące wartosci na.ommit

# lista plików
