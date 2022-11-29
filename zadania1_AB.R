#1. Utwórz projekt na githubie powiąż go z projektem w R.

### 
### Zadanie wykonane na zajęciach.
###


#2. Pociąg z Lublina do Warszawy przejechał połowę drogi ze średnią prędkością 120 km/h.
#Drugą połowę przejechał ze średnią prędkością 90 km/h.
#Jaka była średnia prędkość pociągu.

###
vm<-c(120,90)
s=mean(vm)
print('Srednia prędkość pociągu to:')
print(s)
###


#3. Utwórz funkcję obliczającą współczynnik korelacji r Pearsona dla 2 wektorów o tej samej długości.
#sWczytaj dane plik dane.csv i oblicz współczynnik dla wagi i wzrostu. W komentarzu napisz co oznacza wynik.

###
vm1 <- c(1:10)
vm2 <- c(11:20)

sum_xy<-0
sum_x<-0
sum_y<-0
rP<-0

rPearsona <- function(vm1,vm2){
if (length(vm1)==length(vm2)) {
  vm1_mean=mean(vm1)
  vm2_mean=mean(vm2)
  for (i in 1:length(vm1)){
      sum_xy<-sum_xy+(vm2[i]-vm1_mean)*(vm2[i]-vm2_mean)
      sum_x<-sum_x+(vm1[i]-vm1_mean)**2
      sum_y<-sum_y+(vm2[i]-vm2_mean)**2
  }
  rP<-sum_xy/(sqrt(sum_x)*sqrt(sum_y))
  print(rP)
} else {
  print("Wektory są różnej długości!")
}
}

rPearsona(vm1,vm2)

dane <- read.csv("dane.csv", sep=';')
vm1<-c(dane[,2])
vm2<-c(dane[,1])
rPearsona(vm1,vm2)

# współczynnik korelacji rP zawiera się w przedziale [-1,1]
# Wynik rPearsona = 0.973521 oznacza wysoką, dokładną dodatnią liniową zależność między cechami
# (1 oznacza dokładną dodatnią liniową zależność między cechami)

###


#4. Napisz funkcję zwracającą ramke danych z danych podanych przez użytkownika
#stworzDataFrame <- function(ile=1)
#W pierwszym wierszu użytkownik podaje nazwy kolumn. w kolejnych wierszach zawartość wierszy ramki danych
#( tyle wierszy ile podaliśmy w argumencie ile. ile=1 oznacza, że gdy użytkownik
#nie poda żadnej wartości jako parametr, domyślna wartością będzie 1)

###

# Tu zakładam, że jeżeli użytkownik źle wpisze dane, to komunikatem o błędzie,
# będzie komunikat z konsoli, po próbie wykonania wypisz()

wpisz <- function(sep=",",ile=1){
  linec <-readline(prompt=paste0("Podaj liczbę kolumn (większą od 1 liczbę całkowitą): "))
  ncol <- as.integer(linec)
  line <- readline(prompt=paste0("Podaj nazwy kolumn oddzielone przecinkiem: ",sep,","))
  podz <- strsplit(line,sep)
  liner <-readline(prompt=paste0("Podaj liczbę wierszy (większą od 1 liczbę całkowitą): "))
  nrow <- as.integer(linec)
  lporz=1
  ram <- data.frame(lporz)
  for (i in 1:nrow){
    for (j in 1:ncol){
      rowstr <-readline(prompt=paste0("Podaj wartość (liczbową) kolumny: ", podz[[i]][j]))
      valu <- as.double(rowstr)
      ram[podz[[i]][j]] <- valu
    }  
  }
  ram <- as.data.frame(ram)
  print(ram)
}

wpisz()

###


#5 Napisz funkcję , która pobiera sciezkeKatalogu, nazweKolumny, jakaFunkcje, DlaIluPlikow i liczy: 
#mean, median,min,max w zależności od podanej nazwy funkcji w argumencie, z katologu który podaliśmy
#i z tylu plików ilu podaliśmy dla wybranej nazwy kolumny. 
# UWAGA: w podanych plikach R pobierając komórki nazwane l
#iczbami R wstawi przed nazwy X. Funkcję przetestuj dla katalogu smogKrakow.zip.
#Wykonując obliczenia pomiń brakujące wartości.

###

pobierzDane <- function(sciezka,nazwaKolumny,jakaFunkcja="mean",DlaIluPlikow=1){ 
  sciezka <-readline(prompt=paste0("Podaj nazwę ścieżki: "))
  #  ./smogKrakow2/smogKrakow2/
  nazwaKolumny <-readline(prompt=paste0("Podaj nazwę kolumny: "))
  #  X3_pressure
  jakaFunkcja <- readline(prompt=paste0("Co chcesz policzyć? Wpisz mean, median, min lub max."))
  #  mean
  DlaIluPlikow <- readline(prompt=paste0("Ile plików chcesz użyć do obliczeń? (Podaj liczbę całkowitą pomiędzy 1 a 12"))
  #  3
}

liczZplikow <- function(sciezka,nazwaKolumny,jakaFunkcja="mean",DlaIluPlikow=1){ 
  lista <- list(list.files(sciezka))
  sc1 <- paste(sciezka,lista[[1]][1],sep="")
  for (i in 1:DlaIluPlikow) {
    lista <- list(list.files(sciezka))
    scplik <- paste(sciezka,lista[[1]][i],sep="")
    wartosci  <- read.csv(scplik)
    wartosci <- wartosci[nazwaKolumny]
    wartosci.strings=c("","NA")
    na.omit(wartosci[[nazwaKolumny]])
    if ( i > 1) {
      wartosci <- rbind(dfa,wartosci)
      dfa <- wartosci
      }
  }
  if(jakaFunkcja =="mean"){
    print(mean(wartosci,na.rm = TRUE))
  }
  if(jakaFunkcja=="median"){
    print(median(wartosci,na.rm = TRUE))
  }
  if(jakaFunkcja=="min"){
    print(min(wartosci,na.rm = TRUE))
  }
  if(jakaFunkcja=="max"){
    print(max(wartosci,na.rm = TRUE))
  }
  }

pobierzDane()
jakaFunkcja="max"
liczZplikow(sciezka,nazwaKolumny,jakaFunkcja,DlaIluPlikow)

###
