# ASIA_I

Pakiet do kontroli jakości danych rekrytacyjnych.

[![Travis-CI Build Status](https://travis-ci.org/ZajacT/ASIA1.png?branch=master)](https://travis-ci.org/ZajacT/ASIA1)
[![Coverage Status](https://coveralls.io/repos/ZajacT/ASIA1/badge.svg?branch=master&service=github)](https://coveralls.io/github/ZajacT/ASIA1?branch=master)

## Instalacja

Aby zainstalować pakiet, uruchom w R polecenia:

```r
install.packages('devtools') # potrzbne tylko, gdy nie jest jeszcze zainstalowany
devtools::install_github('ZajacT/ASIA1')
```

## Korzystanie z pakietu

Aby móc użyć pakietu, musisz najpierw załadować go do bieżącej sesji R:

```r
library(ASIA1)
```

Teraz możesz już używać jego funkcji.

### Kontrola spójności danych rekrutacyjnych

Aby sprawdzić spójność danych rekrutacyjnych wpisz w konsoli:

```r
check_data(studia)
```

gdzie `studia` to nazwa zmiennej, na podstawie której będą wyodrębniane grupy, w ramach których obliczone zostaną wyniki. Jeśli przygotowałeś(aś) w zbiorach inną zmienną, po której chciał(a)byś grupować, wpisz jej nazwę zamiast `studia`.

Przy takim wywołaniu funkcji pliki z danymi do wczytania oraz plik, w którym zostaną zapisane wyniki będzie można wskazać poprzez wybór z okna dialogowego.

### Lista kandydatów zagranicznych

Aby uzyskać informacje o zagranicznych kandydatach w konsoli:

```r
foreigners()
```


