install.packages("MASS")
library(MASS)

#5.3
www <- "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/cbe.dat"
CBE <- read.table(www, header = TRUE)
Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)
plot(Elec.ts)
#sa grafika vidimo da disperzija raste vremenom
#pa zbog toga mozemo koristiti logaritamsku transformaciju jer ona resava problem porasta disperzije
#tj. stabilizuje je
#a)
plot(log(Elec.ts), main="Logaritmovani oblik")
#sada sa ovog grafika mozemo reci da se disperzija stabilizovala

#b)
#fitujemo sezonski indikatorski model sa kvadratnim trendom za prirodni logaritam
vreme<-1:length(Elec.ts)
sezone<-cycle(Elec.ts) #kod ove funkcije argument je neka vremenska serija, vraca niz duzina te vremenske serije
#gde je na i-tom mestu neki kvartil koji odgovara i-toj opservaciji
logElec.ts<-log(Elec.ts)
#napravicemo sezonski indikatorski model
Elec.lm<- lm(logElec.ts~ vreme + I(vreme^2) + factor(sezone)) #ako bismo zeleli da ocenimo sve parametre
#potrebno je da dodamo 0 prilikom pravljenja modela
#sada cemo proveriti znacajnost prediktora
summary(Elec.lm)
#vidimo da su svi prediktori znacajni
AIC(Elec.lm)
#dobijamo koeficijent -1591.759

#c)
#fitujemo harmonijski model sa kvadratnim trendom za prirodni logaritam serije
SIN <- COS <- matrix(nr = length(vreme), nc = 6)
for (i in 1:6) {
  COS[, i] <- cos(2 * pi * i * vreme/12)
  SIN[, i] <- sin(2 * pi * i * vreme/12)
}
#pravimo harmonijski model
Elec.harm<-lm(logElec.ts~vreme+I(vreme^2)+SIN[,1]+COS[,1]+SIN[,2]+COS[,2]+SIN[,3]+COS[,3]+SIN[,4]+COS[,4]+SIN[,5]+COS[,5]+SIN[,6]+COS[,6])
summary(Elec.harm)
#vidimo da nisu svi prediktori znacajni
#koristicemo stepwise regresiju za izbor najboljeg model koji je zasnovan na AIC kriterijumu
step(Elec.harm)
#biramo model ciji je AIC=-2722,43
#dobijamo sledece:
Elec.harm1<-lm(logElec.ts ~ vreme + I(vreme^2) + SIN[, 1] + COS[, 1] + SIN[, 2] + COS[, 2] + SIN[, 3] + SIN[, 5] + COS[, 5] + COS[, 6])
#proveravamo znacajnost prediktora
summary(Elec.harm1)
#vidimo da sada svi prediktori jesu znacajni

#d)
#Sada cemo prikazati korelogram i parcijalni korelogram najboljeg modela
#koristicemo AIC kriterijum
AIC(Elec.lm)
AIC(Elec.harm)
AIC(Elec.harm1)
#dobijamo sledece koeficijente: -1591.799, -1589.761, -1596.635 redom
#vidimo da je najvolji Elec.harm1 model
#sada cemo ispitati reziduale za taj model
plot.ts(resid(Elec.harm1))
abline(h=0,col="blue")
#vidimo da se reziduali ponasaju slicno
par(mfrow=c(1,2))
acf(resid(Elec.harm1)) #vidimo da su skoro sve korelacije znacajne
#dovodi do toga da su ocenje vrednosti gresaka manje od stvarnih
pacf(resid(Elec.harm1)) #vidimo da takodje imamo i dosta parcijalnih znacajnih korelacija

#e)
#Za najbolji model treba da fitujemo reziduale jednim AR modelom
harm.ar<-ar(resid(Elec.harm1)) #pomocu funkcije AR fitujemo reziduale AR modelom
harm.ar$order #red je 23, sto znaci da je odgovarajuci model AR(23)
harm.ar$ar #izdvajamo koeficijente

#f)
#korelogram reziduala AR modela
harm.ar
acf(harm.ar$res[-(1:23)]) #vidimo da korelacije nisu statisticki znacajne
#nas prvobitni model nije uspeo da objasni sezonsku komponentu, sto se verovatno desilo zbog toga sto sezonska komponenta
#moze da bude stohasticka, a harmonici su deterministicke funkcije
pacf(harm.ar$res[-(1:23)]) #sa grafika vidimo i da nemamo parcijalnih znacajnih korelacija

#g)treba da zapisemo jednacinu najboljeg modela
summary(Elec.harm1)
vreme<-time(Elec.ts)
mean(vreme) #dobijamo 1974,458
sd(vreme) #dobijamo 9,5383
#racunamo vrednost logaritma pocetne serije u trenutku t
#xt u redu ispod je vrednost logaritma pocetne serije u vremenskom trenutku t
#xt=7,363+7.96*(10^-3)*t - 6.88*(10^-6)*t^2 
#           - 8.84*(10^-2)*cos(2Pi*t/12) - 5.8*(10^-2)*sin(2Pi*t/12)
#           + 1.5*(10^-2)*cos(4Pi*t/12) + 1.51*(10^-2)*sin(4Pi*t/12)
#           - 1.54*(10^-2)*sin(6Pi*t/12) + 1*(10^-2)*cos(10Pi*t/12)
#           + 2.19*(10^-2)*sin(12Pi*t/12) - 7.98*(10^-3)*cos(12Pi*t/12)
#t je vreme

#h)
#iskoristiti najbolji model za prognoziranje potrosnje elektricne energije u periodu 1991-2000
#ukoliko izvrsimo logaritmovanje a zatim eksponencijalnu transformaciju na taj nacin dobijenih predvidjenih vrednosti dolazi do pristrasnosti
# ako je ta pristrasnost mala onda model dobro odgovara podacima
new.time <- seq(length(Elec.ts), length = 120) #pocev od broja length(Elec.ts) predvidjamo u naredne 3 godine
new.data <- data.frame(vreme = new.time, sezone = rep(1:12,10))
predict.lm <- predict(Elec.lm, new.data) #predvidjanje za logaritmovanu seriju, tj. za trend i sezonsku komponentu
predict.arma <- predict(ar(resid(Elec.harm1)), n.ahead = 120) #predvidjanje za AR(23) model
#eksponencijalnom funkcijom vracamo na pocetnu seriju
elec.pred <- ts(exp(predict.lm + predict.arma$pred), start = 1991,freq = 12)
par(mfrow=c(1,1))
ts.plot(cbind(Elec.ts, elec.pred), lty = 1:2)

#5.5
#Hidrolog zeli da prognozira prosecnan mesecni priliv vode(m^3/s) u Font Reservoir za narednih 10 godina
#dati su podaci od 1.1909 do 12.1980

www <- "http://www.math.rs/p/files/69-inflow.txt"
inflow <- read.table(www, header=TRUE)
inflow.ts<-ts(as.vector(t(inflow)), start=1909, freq=12)
plot(inflow.ts, ylab = 'inflow')

#a)
#koristeci linearnu regresiju treba da se prikaze zavisnost dotoka vode od indikatorskih promenljivih kao i promenljive vreme
#treba da se fituje pogodan AR model za reziduale
vreme <- 1:length(inflow.ts)
sezone <- cycle(inflow.ts) 
fr.lm<-lm(inflow.ts~0+vreme+factor(sezone)) #navodimo 0 da bi se izostavio slobodan clan
summary(fr.lm)#vidimo da su sve komponente statisticki znacajne
step(fr.lm)
#vidimo da je ovaj model bolji od manjih modela
acf(resid(fr.lm)) #vidimo da korelogram ima oblik cos funkcije, pa mozemo zakljuciti da je neki AR model u pitanju
pacf(resid(fr.lm)) #vidimo da je samo parcijalna korelacija sa korakom 1 znacajna, pa mozemo zakljuciti da je u pitanju AR(1) model
#sada cemo da fitujemo reziduale
res.ar<-ar(resid(fr.lm))
res.ar$order #vidimo da nije AR(1) vec AR(2) model u pitanju
res.ar$ar
acf(res.ar$res[-c(1,2)])

#b)
#treba da prikazemo histagram reziduala AR modela
hist(res.ar$res[-c(1,2)]) #vidimo da reziduali nisu simetricni oko nule
#sada cemo da fitujemo back-to-back Vejbulovu raspodelu greskama
resids<-res.ar$res[-c(1,2)]
library(MASS)
pos.resids <- resids[resids > 0]  # subset the positive residuals
neg.resids <- resids[resids < 0]  # subset the negative residuals
neg.resids <- neg.resids * -1  # transform the negative residuals
pos.Weibull <- fitdistr(pos.resids, "weibull")  # fit a Weibull to the positive residuals
neg.Weibull <- fitdistr(neg.resids, "weibull")  # fit a Weibull to the (transformed) negative reisduals

#c) simulirati 20 10-godisnjih realizacija za inflow
#serija inflow ima trend objasnjen promenljivom t, sezonske komponente objasnjene indikatorskim promenljivim i slucajnu komponentu objasnjenu AR(2) modelom
new.t<-seq(1981,len=10*12,by=1/12)
mean(time(inflow.ts)) #dobijamo 1944.458
sd(time(inflow.ts)) #dobijamo 20.50796
sezonskekomponente<-rep(fr.lm$coefficients[-1],10)
#sada treba da simuliramo seriju reziduala
length(resids) #dobijamo 850
length(pos.resids) #dobijamo 343
length(neg.resids) #dobijamo 507
#u novoj seriji bi trebalo da ima  343/850*120= 48.42353, dakle oko 48 pozitivnih, iz raspode koja je dobijena uklapanjem pos.resids u Vejbulovu, a preostalih 72 iz raspodele koja je dobijena uklapanjem neg.resids u Vejbulovu
#...

#d)
#razlozi zasto logaritamska transformacija moze biti pogodna i za inflow seriju
plot(inflow.ts, ylab = 'inflow') #vidimo da je varijansa nestabilna, i da postoje jako velike vrednosti varijanse
#kako su u pitanju sve pozitivne vrednosti, svakako mozemo primeniti transformaciju
plot(log(inflow.ts),ylab = 'log(inflow)') #medjutim vidimo da se i nije nesto znacajno promenila

#e)
#Regress log(inflow) koristeci indikatorske promenljive kao i promenljivu vreme 
#i fituj pogodan AR model za seriju reziduala
fr.lm<-lm(log(inflow.ts)~0+vreme+factor(sezone)) #da bi nam se izgubio slobodan ;lan stavljamo 0
summary(fr.lm) #svi su znacajni
step(fr.lm) #vrednost AIC=-312.44
#sada crtamo korelogram i parcijalni korelogram
acf(resid(fr.lm)) #korelogram opet ima oblik cos funkcije, pa je u pitanju neki AR model
pacf(resid(fr.lm)) #vidimo da imamo parcijalnih znacajnih korelacije
res.ar<-ar(resid(fr.lm))
res.ar$order #u pitanju je AR(2) model
res.ar$ar
acf(res.ar$res[-c(1,2)]) #vidimo da AR model dobro objasnjava reziduale

#f)
#prikazati histagram reziduala
hist(res.ar$res[-c(1,2)])
#sada cemo da fitujemo back-to-back Vejbulovu raspodelu gresaka
resids<-res.ar$res[-c(1,2)]
pos.resids <- resids[resids > 0]  # subset the positive residuals
neg.resids <- resids[resids < 0]  # subset the negative residuals
neg.resids <- neg.resids * -1  # transform the negative residuals
pos.Weibull <- fitdistr(pos.resids, "weibull")  # fit a Weibull to the positive residuals
neg.Weibull <- fitdistr(neg.resids, "weibull")  # fit a Weibull to the (transformed) negative reisduals

#g)...
#h)...

#6.4

install.packages("datasets")
library(datasets)

data(AirPassengers)
AP <- AirPassengers
AP
plot(AP)
plot(log(AP))

#a)
#treba da fitujemo pogodan regresioni model za ovu seriju
SIN <- COS <- matrix(nr = length(AP), nc = 6)
for (i in 1:6) {
  SIN[, i] <- sin(2 * pi * i * time(AP))
  COS[, i] <- cos(2 * pi * i * time(AP))
}
vreme <- (time(AP) - mean(time(AP)))/sd(time(AP))
sd(time(AP)) #dobijamo 3.476109
#prvo cemo napraviti model sa svim prediktorima
AP.lm1 <- lm(log(AP) ~ vreme + I(vreme^2) + I(vreme^3) + I(vreme^4) + SIN[,1] + COS[,1] + SIN[,2] + COS[,2] + SIN[,3] + COS[,3] +SIN[,4] + COS[,4] + SIN[,5] + COS[,5] + SIN[,6] + COS[,6])
summary(AP.lm1) #vidimo da nisu svi prediktori znacajni, pa je potrebno poboljsati nas model
step(AP.lm1)
#dobijamo sledeci model, ciji je ACI najmanji i iznosi -864.12
AP.lm2<-lm(log(AP) ~ vreme + I(vreme^2) + I(vreme^4) + SIN[, 1] + COS[, 1] + SIN[, 2] + COS[, 2] + SIN[, 3] + COS[, 3] + SIN[, 4] + 
             COS[, 4] + SIN[, 5])
summary(AP.lm2) #medjutim vidimo da i ovde svi prediktori nisu znacajni, tacije COS[, 3]
#izbacicemo njega iz modela pri cemu dobijamo
Ap.lm3<-lm(log(AP) ~ vreme + I(vreme^2) + I(vreme^4) + SIN[, 1] + COS[, 1] + SIN[, 2] + COS[, 2] + SIN[, 3]  + SIN[, 4] + 
             COS[, 4] + SIN[, 5])
summary(Ap.lm3) # u ovom modelu su svi prediktori znacajni
#sada cemo nacrtati korelogram i parcijalni korelogram:
par(mfrow=c(1,2))
acf(resid(AP.lm2)) #imamo dosta znacajnih korelacija
pacf(resid(AP.lm2)) # ukazuje na to da je vrv beli sum
par(mfrow=c(1,1))

#b)
#treba da uklopimo reziduale iz dela pod a) u ARMA(p,q) model, gde su p,q<=2
best.order <- c(0, 0, 0)
best.aic <- Inf
for (i in 0:2) for (j in 0:2) {
  fit.aic <- AIC(arima(resid(AP.lm2), order = c(i, 0,
                                                j)))
  if (fit.aic < best.aic) {
    best.order <- c(i, 0, j)
    best.arma <- arima(resid(AP.lm2), order = best.order)
    best.aic <- fit.aic
  }
  
}

best.order #dobili smo ARMA(1,2) model
#uporedicemo AIC nasih modela:
AIC(AP.lm2) #dobijamo -453.4617
AIC(best.arma) #dobijamo -556.0312
#vidimo da je bolji best.arma model jer ima manju AIC vrednost
#korelogram
acf(resid(best.arma)) #vidimo da imamo 3 znacajne korelacije

#c)
#treba da se prognozira broj putnika koji putuju avionom u 1961. godini
new.t <- time(ts(start = 1961, end = c(1961, 12), fr = 12))
vreme <- (new.t - mean(time(AP)))/sd(time(AP))
SIN <- COS <- matrix(nr = length(new.t), nc = 6)
for (i in 1:6) {
  COS[, i] <- cos(2 * pi * i * new.t)
  SIN[, i] <- sin(2 * pi * i * new.t)
}
SIN <- SIN[, -6]
new.dat <- data.frame(vreme = as.vector(vreme), SIN = SIN, COS = COS)
AP.pred.ts <- exp(ts(predict(AP.lm2, new.dat), start = 1961, fr=12))
ts.plot(log(AP), log(AP.pred.ts), lty = 1:2)
#vidimo da se model dobro uklapa

#6.5
#a)
#napisati funkciju koja racuna acf za ARMA(1,1) proces. 
#u slucaju da nam nisu zadati alfa i beta i k, nego da imamo samo seriju dobijamo ih iz:
# alfa<-coef(arima(vs, order = c(1, 0, 1)))[1]
# beta<-coef(arima(vs, order = c(1, 0, 1)))[2]
# k=length(vs)

acfun <- function(k,alfa,beta)
{
  rho.k <- rep(0,k)
  rho.k[1] <- 1
  for (i in 2:k)
    rho.k[i] = alfa^(i-1)*(alfa+beta)*(1+alfa*beta)/(1+alfa*beta+beta^2)
  return(rho.k)
}

#b)
# graficki predstaviti funkciju za alfa=0.7, beta=-0.5, za korake od 0 do 20
plot(acfun(20,0.7,-0.5),xlab = "Lag",ylab = "ACF", type = "h")
abline(h=0)

#c)
#simulirati 100 vrednosti ARMA(1,1) modela sa gore zadatim koeficijentima i nacrtati korelogram
sim100<-arima.sim(100,model=list(order(1,0,1),ar=c(0.7),ma=c(-0.5)))
acf(sim100)


#7.4
#broj prekookeanskih posetilaca koji su dolazili u Novi Zeland, zabelezen mesecni broj u periodu od 1977. do 1995. godine 
www <- "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/osvisit.dat"
ovsisit <- read.table(www, header = TRUE)
ovsisit.ts<-ts(as.vector(t(ovsisit)), start=1977, freq=12, end=1995)
plot(ovsisit.ts)
plot(log(ovsisit.ts))

#a)
#korelogram za seriju ovsisit.ts
acf(log(ovsisit.ts)) #vidimo da su sve korelacije znacajne
#vidimo da je periodican, sa korakom 12, pa postoji jaka sezonska komponenta

#b)
#uklopiti u ARIMA(1,1,0) model
modelarima<-arima(ovsisit.ts,order=c(1,1,0))
modelarima
acf(resid(modelarima)) #imamo znacajnih korelacija

#c)
#uklopi u sezonski ARIMA(1,1,0)(0,1,0)_12
install.packages("forecast")
library(forecast)

fit<-Arima(ovsisit.ts, order = c(1,1,0), seasonal = c(0,1,0), lambda = 0)
acf(fit$residuals) #imamo znacajnih korelacija
tsdisplay(residuals(fit))
Box.test(residuals(fit), lag=36, fitdf=6, type="Ljung")
#vidimo da je p vrednost mala

#d)
#u slucaju kada je lamda=0 u Box Cox transformaciji, tada se javlja logaritmovanje
fit1<-Arima(ovsisit.ts, order = c(1,1,0), seasonal = list(order=c(1,1,0),12), lambda = 0)
fit2<-Arima(ovsisit.ts, order = c(0,1,1), seasonal = list(order=c(0,1,1),12), lambda = 0)
fit3<-Arima(ovsisit.ts, order = c(1,1,0), seasonal = list(order=c(0,1,1),12), lambda = 0)
fit4<-Arima(ovsisit.ts, order = c(0,1,1), seasonal = list(order=c(1,1,0),12), lambda = 0)
fit5<-Arima(ovsisit.ts, order = c(1,1,1), seasonal = list(order=c(1,1,1),12), lambda = 0)
fit6<-Arima(ovsisit.ts, order = c(1,1,1), seasonal = list(order=c(1,1,0),12), lambda = 0)
fit7<-Arima(ovsisit.ts, order = c(1,1,1), seasonal = list(order=c(0,1,1),12), lambda = 0)
par(mfrow=c(2,4))
acf(residuals(fit1))
acf(residuals(fit2))
acf(residuals(fit3))
acf(residuals(fit4))
acf(residuals(fit5))
acf(residuals(fit6))
acf(residuals(fit7))
par(mfrow=c(1,1))
AIC(fit1) #dobijamo -498.0122
AIC(fit2) #dobijamo -529.8248
AIC(fit3) #dobijamo -506.3871
AIC(fit4) #dobijamo -524.4496
AIC(fit5) #dobijamo -533.3003
AIC(fit6) #dobijamo -527.3139
AIC(fit7) #dobijamo -588.2416
#kako nam je u 5. modelu AIC najmanji, taj model nam je najbolji

#e)
# ARIMA(p,d,q)(P,D,Q)s

# TETA_P*(B^s)teta_p(B)(1 − B^s)^D(1 − B)^d*x_t = FI_Q(B^s)fi_q(B)w_t


# AR:  teta(B) = 1 - teta_1*B - ... - teta_p*B^p
# MA:  fi(B) = 1 + fi_1*B + ... + fi_q*B^q

# The seasonal components are:
# Seasonal AR:  TETA(B^s) = 1 - TETA_1*B^s - ... - TETA_P*B^(Ps)
# Seasonal MA:  FI(B^s) = 1 + FI_1*B^s + ... + FI_Q*B^(Qs)
# s=12, D=d=1

# (1-0.2492B^12)(1-0.2562B)(1-B^12)(1-B)z_t = (1+0.1352B^12)(1+0.0589B)w_t

#f)
#testiranje reziduala na stacionarnost
acf(fit5$residuals)
plot(fit5$residuals) #mozemo reci da vazi stacionarnost

#g)
#predvidjanje za sve mesece 1996. godine
predvidjanje<-predict(fit5,start=1996,n.ahead = 12)
predvidjanje$pred
#ukupno
sum(exp(predvidjanje$pred+fit5$sigma2/2))

#7.6
#podaci o berzi u  7 gradova, od 6.1.1986. do 31.12.1997. 
www <- "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/stockmarket.dat"
x <- read.table(www, header = TRUE)
x[1:3,]
attach(x)
amsterdam.ts <- ts(x$Amsterdam)

#a)
#prikazati seriju Amsterdam i diff(Amsterdam)
par(mfrow = c(1,2))
plot(Amsterdam) #rastuci trend
plot(diff(Amsterdam)) 
par(mfrow = c(1,1))

#b)
#treba fitovati neke ARIMA modele Amsterdam seriji i videti koji je najprihvatljiviji
AIC(arima(Amsterdam,order = c(0,1,0))) #dobijamo 18571.77
AIC(arima(Amsterdam,order = c(1,1,0))) #dobijamo 18573.77
AIC(arima(Amsterdam,order = c(0,1,1))) #dobijamo 18573.77
AIC(arima(Amsterdam,order = c(1,1,1))) #dobijamo 18575.77
#najmanji AIC ima ARIMA(0,1,0) pa njega biramo kao najbolji

#c)
#predstaviti korelogram reziduala najboljeg modela kao i korelogram kvadrata reziduala
arima010<-arima(amsterdam.ts, order = c(0, 1, 0))
par(mfrow=c(1,2))
acf(arima010$residuals) #imamo dosta statisticki znacajnih korelacija
acf(arima010$residuals^2) #vidimo da su sve orelacije statisticki znacajne
par(mfrow=c(1,1))

#d)
#treba fitovati neke GARCH modele rezidualima prethodnih modela i videti koji je najprihvatljiviji
install.packages("tseries")
library(tseries)

a<-resid(arima010)
AIC(garch(a,order = c(0,1)))#17613.54
AIC(garch(a,order = c(1,0)))
AIC(garch(a,order = c(1,1)))# 16142.38
AIC(garch(a,order = c(0,2)))#16939.75
#vidimo da najmanji AIC ima treci model, pa ce nam on biti najbolji

#e)
#korelogram reziduala najboljeg modela kao i korelogram kvadrata reziduala
garch11<-garch(a,order=c(1,1))
par(mfrow=c(1,2))
acf(resid(garch11)[-1]) #imamo 3 znacajne korelacije, malo podseca na beli sum
acf((garch11$residuals[-1])^2) #nemamo znacajnih korelacija
