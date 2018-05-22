# Główny plik 

# Tu będę wykonywał i opisywał kolejne etapy pracy


# Wgranie nieprzetworzonych plików w postaci w jakiej są eksportowane z IRK
  # Nie będę tu śmiecił swoimi ścieżkami dostępu
  
  # pliki jakie trzeba wgrać
    # rej - spis rejestracji
    # pkt - wyniki rekrutacyjne na poszczególne 
    # mat - wyniki maturalne i z innych egzaminów
    # slo - slownik kierunków

# Pkt są częściowo powtórzeniem danych z rej. Nowe informacje to wynik i pozycja na liście.

pkt<-pkt %>% select(pesel,studia,wynik,pozycja_na_liscie)
rej<-left_join(rej,pkt)


# Kontrola danych na jednym kierunku

# wyliczanie poszczególnych statystyk - zmienna studia może być zastąpiniona inną np. zbijającą tury w jedną
# docelowo raporty dla:
  #- każdego kodu kierunku z IRK (osobno tury, typy rekrutacji, np. S1-SC-C-III)
  #- każdego kierunku z zachowaniem informacji o typie rekrutacji, ale łącznie dla wszystkich tur (S1-SC-C i S1-SC-C-II są traktowane łącznie)
  #- każdego kierunku ignorując tury i rekrutację dla obcokrajowców ( czyli S1-SC-C i S1-SC-II itd. są zapisywane razem jako S1-SC), 

# na tym etapie nie interesuje nas to, czy ktoś kilkukrotnie się zapisał 

# liczba osób zarejestrowanych
NREJ<-nrej(rej,"studia")

# liczba osób opłaconych
NKAN<-nkan(rej,"studia")

# statusy zakwalifikowania
NZAK<-nzak(rej,"studia")

# statusy przyjęcia
NPRZ<-nprz(rej,"studia")

# liczba osób z niepoprawnym - ujemnym wynikiem rekrutacyjnym 
NBLPKT<-nblpkt(rej,"studia")

# liczba osób z zakwalifikowaniem, ale bez opłaty
NBLZAKKAN<-nblzakkan(rej,"studia")

# liczba osób z przyjęciem, ale bez zakwalifikowania
NBLPRZZAK<-nblprzzak(rej,"studia")

# liczba osób niezakwalifikowanych, mimo że wniosły opłatę i mają wynik rekrutacyjny wyższy niż ostatni zakwalifikowany
NBLZAKPKT<-nblzakpkt(rej,"studia")

# teraz trzeba to skleić. 