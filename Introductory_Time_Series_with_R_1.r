#1.a)
install.packages("forecast")
www<-"http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/global.dat" #ocitavamo bazu podataka o prosecnoj temperaturi
Global<-scan(www) #ucitavamo u niz
summary(Global)
class(Global) #vidimo da je klasa numeric

Global.ts<-ts(Global,frequency=12,start=c(1856,1),end=c(2005,12)) #pravimo vs koja predstavnja mesecne vrednosti
start(Global.ts) #pocetak serije
end(Global.ts) #kraj serije
plot(Global.ts) 
Global.ts
#pravimo sravnjanu vs, od mesecne pravimo godisnju seriju
Global.annual.ts<-aggregate(Global.ts, FUN=mean) #funkcija aggregate izdvaja samo onaj period kritican za globalno zagrevanje
Global.annual.ts
par(mfrow=c(1,2))
#graficki prikaz mesecne i godisnje vs
plot(Global.ts) 
plot(Global.annual.ts)
#
boxplot(Global.ts~cycle(Global.ts)) #boxplot sa prosecnim temperaturama za svaki mesec pojedinacno
#finkcija cycle odredjuje poziciju u ciklusu, tj sve vrednosti deli u 12 kategorija i crta boxplotove
#posto nam je samo deo od 1970. godine kritican za globalno zagrevanje, posmatracemo samo taj deo serije
Global.ts<-window(Global.ts, start=c(1970,1))
plot(Global.ts)
New.time<-time(Global.ts) #funkcija time u niz stavlja sve vremenske trenutke u kojima su izvrsena merenja
abline(coef(lm(Global.ts~New.time)))


#b)
#posmatracemo i aditivni i multiplikativni model
Global.decom.add<-decompose(Global.ts) #aditivni model
plot(Global.decom.add)
Global.decom.mult <- decompose(Global.ts, type = "mult") #multiplikativni model
plot(Global.decom.mult)
#kako sezonska komponenta ne raste sa porastom trenda, koristimo aditivni model
Trend <- Global.decom.add$trend #prikazujemo samo trend
plot(Trend)
Seasonal <- Global.decom.add$seasonal #prikazujemo samo sezonsku komponentu
plot(Seasonal)
layout(1:2)
ts.plot(cbind(Global.ts, Trend)) #graficki prikaz stvarni podaci i ocenjeni trend
ts.plot(cbind(Trend,Trend+Seasonal)) #graficki prikaz trenda i trenda i sezonske komponente

#c)
#acf funkcija proverava da li je model odgovarajuci na osnovu korelograma
#dozviljeno je da 5% korelacija upadne u kriticnu oblast
plot(Global.ts) 
acf(Global.ts) #vidimo da korelacije sporo opadaju sto je posledica rastuceg trenda
acf(Global.ts)$acf #mozemo dobiti i odgovarajuci niz brojeva
acf(Global.ts)$acf[1] #vrednost autokorelacione funkcije u nuli je jedan, sto je upravo vrednost prvog elementa niza
acf(Global.ts, type = "covariance")$acf[2] #vrednost autokovarijacione funkcije dobijamo kada dodamo jos jedan element niza
n<-(length(Global.decom.add$random)-6)
acf(Global.decom.add$random[7:n]) #korelogram reziduala
#vidimo da ima oblik funkcije kosinusa sto znaci ili da je model los ili da je model procesa greska AR(2) model
#takodje mozemo videti da je najveca pozitivna korelacija sa korakom 1, a najveca negativna korelacija je sa korakom 5
sd(Global.ts[7:138])
sd(Global.ts[7:138] - Global.decom.add$trend[7:138])
sd(Global.decom.add$random[7:138])
#uklanjanje deterministickih komponenti dovodi do smanjenja disperzije
#a posto je disperzija mala, mozemo zakljuciti da je model dobar i da je debojeni oblik korelograma posledica AR(2) modela

#d)
#aditivni
Global.hw <- HoltWinters (Global.ts)
Global.hw #ispisuju se parametri modela i koeficijenti (vrednosti koje sluze za predvidjanje)
Global.hw$coef #mogu i ovako koeficijenti da se dobiju
Global.hw$SSE #minimizirana greska predvidjanja jedan korak unapred
sqrt(Global.hw$SSE/length(Global))
sd(Global)
head(Global.hw$fitted)
plot(Global.hw) #crna linija nam je pocetna serija, a crvena je serija izravnjana HW metodom
#Kako je vrednost alfa jednaka 0.519, mozemo reci da su obe ocene otprilike jednake
#kako je vrednost bete mala kod aditivnog modela, to znaci da se za vrednost nagiba uzima poslednja ocenjena vrednost, sto znaci, ocenjeno je da se nagib trenda
#sporo menja za razliku od nivoa sezonske varijacije
#vrednost game je 0,3518

#e)
#Osnovna funkcija za prognoziranje - predict
layout(1:2)
Global.hw.predict<-predict(Global.hw, n.ahead=5*12)
ts.plot(Global.ts,Global.hw.predict,lty=1:2)
plot(cbind(Global.ts,Global.hw.predict)) #graficki prikaz pocetne serije i predvidjanja
#Drugi nacin za predvidjanje je preko paketa forecast, tada dobijamo i intervale poverenja
library(forecast)
Global.hw2 <- forecast.HoltWinters(Global.hw, h=5*12)
plot(Global.hw2)
#treba jos proveriti da li je model dobar, tj. da li nema serijske korelacije u seriji reziduala, da li su reziduali normalno raspodeljeni i da li je disperzija serije reziduala konstantna tokom vremena
#koristimo forecast jer su tu reziduali ugradjeni
#prvo proveravamo postojanje serijske korelacije
acf(Global.hw2$residuals) #grafik korelograma da ima znacajnih korelacija
#takodje mozemo koristiti i Ljung-Box test za proveru postojanosti znacajnih korelacija
Box.test(Global.hw2$residuals, type="Ljung-Box")
#Nulta hipoteza je da su sve korelacije jednake 0, a kako je p vrednost testa velika, prihvatamo nultu hipotezu
#Sada cemo da proverimo da li su reziduali normalno rasporedjeni
qqnorm(Global.hw2$residuals)#sa grafika mozemo pretpostaviti da jesu normalno rasporedjeni
#jos nam je ostalo da proverimo konstantnost disperzije?
plot(Global.hw2$residuals)

#2.
layout(1:2)
#mi nam je nepoznat parametar
#kako je za beli sum ocekivanje nula, nepristrasna ocena za mi ce biti uzoracka sredina
greska1<-function(mi){
  w<-rnorm(100)
  x<-mi+w[1]
  for (t in 2:100) x[t] <- mi+w[t]+0.8*w[t-1]
  ts.plot(x)
  abline(h=mean(x))
  return(mi-mean(x))
}
greska1(10)
#i u drugom slucaju nam je mi nepoznat parametar, i dalje je beli sum, sto znaci da je opet nepristrasna ocena za mi uzoracka sredina
greska2<-function(mi){
  x2<-w2<-rnorm(100)
  for (t in 1:100) x2[t] <-mi+w2[t]
  ts.plot(x2)
  abline(h=mean(x2))
  return(mi-mean(x2))
}
greska2(10)
#Na primer, uzecemo razlicite vrednosti za mi i posmatrati sta se desava sa greskama
abs(greska1(10))-abs(greska2(10))
abs(greska1(-8))-abs(greska2(-8))
abs(greska1(3))-abs(greska2(3))
abs(greska1(0.25))-abs(greska2(0.25))
#da bismo uporedili ove dve greske, mozemo da izracunamo njihovu disperziju, pa da uporedimo
#za x_t<-mi+w_t+0.8*w_(t-1):
#za reaizovani uzorak x1,...,xn imamo da je x1=mi+w1, x2=mi+w2+0.8*w1,...,xn=mi+w_n+0.8*w_(n-1)
#kako smo vec rekli da je ocena za mi uzoracka sredina, dobijamo
#mi_ocena1=mean(X)=(X1+...Xn)/n=(mi+w1+mi+w2+0.8*w1+...+mi+w_n+0.8*w_(n-1))/n=(n*mi/n+(1,8*w1+...+1.8w_(n-1)+w_n)/n
#D(mi_ocena1)=D((n*mi)/n+(1,8*w1+...+1.8w_(n-1)+w_n)/n)=D(1,8*w1+...+1.8w_(n-1)+w_n)/n)=...=(3.24*sigma^2)/n-(2.24sigma^2)/(n^2)


#Za x_t=mi+w_t:
#za realizovani uzorak x1,x2,...,xn imamo x1=mi+w1,...,xn=mi+w_n
#mi_ocena2=mean(x)=mi+mean(w)
#D(mi_ocena2)=D(mean(w))=(sigma^2)/n
#D(mi_ocena1)-D(mi_ocena2)=(2.24*sigma^2)*(1/n-1/(n^2))>0
#vidimo da je vece standardno odstupanje u prvom slucaju

#3.
simulacija<-function(A, omega, fi){
  set.seed(1)
  w<-rnorm(500)
  x<-vector()
  for (t in 1:500) x[t] <-A*cos(2*pi*omega*t+fi)+w[t]
  plot(x, type = "l")
  return(x)
}
simulacija(1,1/2,0) #ovde dobijamo cos(pi*t)+w_t, omega je ceo broj, a cos(pi*t) uzima vrednosti -1 i 1, pa funkcija periodicno menja znak, sa periodom 2
simulacija(1,1,0) #ovde se dobija cos(2*pi*t)+w_t, a kako je cos(2*pi*t) periodicna, ostaje samo w_t, jer je A=1
simulacija(1,2,5) #dobijamo sada cos(2*pi*t+5)+w_t, kako je omega ceo broj ostaje samo cos5+w_t
simulacija(1, 1/12, 120) #dobijamo cos((1/6)*pi*t+120)+w_t 
simulacija(1,1/3,0) #bice funkcija sa periodom 3
#crtamo korelograme
acf(simulacija(1,1/2,0)) #vidimo da je postoji jaka korelisanost izmedju svake 2, takodje vidimo da je pozitivna pri parnim koracima, a negativna pri neparnim  
acf(simulacija(1,1,0)) #vidimo da nisu korelisane
acf(simulacija(1,2,5)) #i ovde takodje se vidi da se odbacuje H0, tj da nisu korelisane
acf(simulacija(1,1/2,120)) #ovde se takodje javlja velika korelisanst
acf(simulacija(1,1/3,0)) #vidimo da postoji korelisanost, najveca je pri koracima 3k, dok je najmanja u ostalim koracima
#mozemo primetiti da parametar omega najvise utice na korelisanost, tj kada je on ceo broj nema korelacije, dok u suprotnom se javlja jaka korelisanost
#sada posmatramo specijalan slucaj prethodnog modela, tj x_t=s_t+w_t, gde je s_t=cos(2*pi*t/5)
#Dakle, imamo da je A=1, omega=1/5 i fi=0
#kako vidimo da omega nije ceo broj, po prethodnom slucaju bi trebalo da postoji jaka korelisanost, pa cemo to i proveriti
serija<-simulacija(1,1/5,0) #vidimo da je period jednak 5
acf(serija) #vidimo da postoji korelaciona veza zaista postoji, takodje mozemo videti i da je fkrekvencija jednaka 5
#metod pokretnih proseka je metod izravnanja serije, za ocenu trenda u tacki t
class(serija) #prvo treba da konvertujemo u ts
serija.ts<-ts(serija, frequency = 5)
serija.ts
# sada koristicemo funkciju decompose
serija.decompose<-decompose(serija.ts)  
serija.decompose
plot(serija.decompose)
trend<-serija.decompose$trend #izdvajamo trend
trend
plot(trend)
Seasonal<-serija.decompose$seasonal #sezonska komponenta
plot(Seasonal)
plot(serija.ts)
ts.plot(cbind(serija.ts, trend), type="l", lty=c(1,2)) #graficki prikaz pocetne serije i trenda
#model izravnate serije, teorijski gledano, je specijalan slucaj...?

#4.
set.seed(1)
TIME<-1:(15*12)
w<-rnorm(15*12, sd = 0.5)
#biramo neku proizvoljnu seriju
s<-0.8*cos(2*pi*TIME/12+120) 
x<-s+w
plot(x,type="l",xlab="Time")
acf(x) #sa korelograma vidimo da je period stvarno 12
# X_t=A*cos(2*pi*omega*t+fi)+W_t 
# A*cos(2*pi*omega*t+fi)=A*cos(2*pi*omega*t)*cos(fi)-A*sin(2*pi*omega*t)*sin(fi)
# =alfa*cos(2*pi*omega*t)+beta*sin(2*pi*omega*t), gde je alfa=A*cos(fi), beta=-A*sin(fi)
#pravimo matrice
SIN <- COS <- matrix(nr = length(TIME), nc = 6)
for (i in 1:6) {
  COS[, i] <- cos(2 * pi * i * TIME/12)
  SIN[, i] <- sin(2 * pi * i * TIME/12)
}

model<-lm(x~COS[, 1]+SIN[, 1]+COS[, 2]+SIN[, 2]+COS[, 3]+SIN[, 3]+COS[, 4]+
            +SIN[, 4]+COS[, 5]+SIN[, 5]+COS[, 6]+SIN[, 6])
model
summary(model)#vidimo da su znacajni prediktori COS[, 1],SIN[, 1],SIN[, 5]
#zato cemo sada napraviti poboljsani model koji ce da sadrzi samo ova tri prediktora jer su ona znacajna
modelPoboljsani<-lm(x~0+COS[, 1]+SIN[, 1]+SIN[, 5])#potrebno je eksplicitno navesti nulu da u modelu ne bi postojao slobodan clan, tj intercept a ni on nije znacajan
#napravicemo jos jedan model koji ne sadrzi jedan od ova tri znacajna prediktora
summary(lm(x~0+COS[, 1]+SIN[, 1]))
#pomocu funkcije AIC cemo izvrsiti poredjenje ova dva modela, tako da onaj model za koju je vrednost AIC funkcije manja jeste bolji
AIC(lm(x~0+COS[, 1]+SIN[, 1]))
AIC(modelPoboljsani) #kako je ovde vrednost AIC funkcije manja, ovaj model nam je bolji
y<-modelPoboljsani$coefficients[1]*cos(2*pi*TIME/12)+modelPoboljsani$coefficients[2]*sin(2*pi*TIME/12)+
  modelPoboljsani$coefficients[3]*sin(2*pi*5*TIME/12)
plot(x,type="l",xlab="Time")
points(TIME, y, type = "l", col="blue")#vidimo da nas model i nije bas najbolji, sto mozemo zakljuciti i iz cinjenice da je Adjusted R-squared:  0.6166, tj sto je ova vrednost veca i nas poboljsani model ce biti bolji
#sada ovo primenjujemo na konkretne primere iz zadatka
www<-"http://www.math.rs/p/files/69-regr1.txt"
regr1<-scan(www)
regr1
regr1.ts<-ts(regr1)
regr1.ts
plot(regr1.ts)
acf(regr1.ts) #vidimo da je peridicnost jednaka 8
TIME<-time(regr1.ts) #funkcija vremena
#pravimo matrice
SIN <- COS <- matrix(nr = length(TIME), nc = 6)
for (i in 1:6) {
  COS[, i] <- cos(2 * pi * i * TIME/8)
  SIN[, i] <- sin(2 * pi * i * TIME/8)
}
model1<-lm(regr1.ts~COS[, 1]+SIN[, 1]+COS[, 2]+SIN[, 2]+COS[, 3]+SIN[, 3]+COS[, 4]+
             +SIN[, 4]+COS[, 5]+SIN[, 5]+COS[, 6]+SIN[, 6])
model1
summary(model1) #vidimo da su nam ovde znacajni prediktori COS[, 1],SIN[, 1],COS[, 2]
#takodje primecujemo da nam se za prediktore COS[, 5],SIN[, 5],COS[, 6],sin[, 6] javljaju NA vrednosti, medjutim, posto ti prediktori nisu znacajni njih cemo svakako izbaciti, pa to ne utice na nas
#sada pravimo poboljsani model koji ce da sadrzi samo znacajne prediktore
model1.poboljsanje<-lm(regr1.ts~COS[, 1]+SIN[, 1]+COS[, 2])
summary(model1.poboljsanje)
#kako nismo ukljucili 0, javlja nam se i interception, a on nam nije znacajan pa cemo i njega izbaciti
model1.poboljsanje2<-lm(regr1.ts~0+COS[, 1]+SIN[, 1]+COS[, 2])
summary(model1.poboljsanje2) #sada nas model sadrzi samo znacajne prediktore
y<-model1.poboljsanje2$coefficients[1]*cos(2*pi*TIME/8)+model1.poboljsanje2$coefficients[2]*sin(2*pi*TIME/8)+
  model1.poboljsanje2$coefficients[3]*sin(2*pi*2*TIME/8)
plot(regr1.ts,type="l",xlab="Time")
points(TIME, y, type = "l", col="blue")#vidimo da je Adjusted R-squared:  0.5754
#sada cemo pomoci korelograma proveriti da li je nas model dobar
acf(model1.poboljsanje2$residuals)#vidimo da manje od 5% upada u kriticnu oblast, pa prihvatamo nultu hipotezu
#mozemo proveriti jos i normalnost
qqnorm(model1.poboljsanje2$residuals)#mozemo pretpostaviti da jeste u pitanju normalna raspodela
plot(model1.poboljsanje2$residuals)#mozemo pretpostaviti i konstantnost disperzije

#sada isto ovo proveravamo za drugi primer iz zadatka
www2<-"http://www.math.rs/p/files/69-regr2.txt"
regr2<-scan(www2)
regr2
regr2.ts<-ts(regr2)
regr2.ts

plot(regr2.ts)

regr2.ts2<-ts(regr2, frequency = 8)
regr2.decompose<- decompose(regr2.ts2)
plot(regr2.decompose)
acf(regr2.ts) #frekvencija je 8

model2<-lm(regr2.ts~COS[, 1]+SIN[, 1]+COS[, 2]+SIN[, 2]+COS[, 3]+SIN[, 3]+COS[, 4]+
             +SIN[, 4]+COS[, 5]+SIN[, 5]+COS[, 6]+SIN[, 6])
model2
summary(model2)#znacajni prediktori su COS[, 1],SIN[, 1]
#sada pravimo poboljsani model
model2.poboljsanje<-lm(regr2.ts~COS[, 1]+SIN[, 1])
summary(model2.poboljsanje)
#pravimo model bez intercept-a
model2.poboljsanje2<-lm(regr2.ts~0+COS[, 1]+SIN[, 1])
summary(model2.poboljsanje2)
y<-model2.poboljsanje2$coefficients[1]*cos(2*pi*TIME/8)+model2.poboljsanje2$coefficients[2]*sin(2*pi*TIME/8)
plot(regr2.ts,type="l",xlab="Time")
points(TIME, y, type = "l", col="blue")
#medjutim kako je ovde Adjusted R-squared:  0.3622 , ta vrednost nam je mala, sto takodje mozemo i videti sa grafika da nam model nije dobar
#pa moramo da uvedemo TIME prediktore do drugog stepena
model3<-lm(regr2.ts~TIME+I(TIME^2)+COS[, 1]+SIN[, 1]+COS[, 2]+SIN[, 2]+COS[, 3]+SIN[, 3]+COS[, 4]+
             +SIN[, 4]+COS[, 5]+SIN[, 5]+COS[, 6]+SIN[, 6])
summary(model3)

model3.poboljsanje<-lm(regr2.ts~TIME+I(TIME^2)+COS[, 1]+SIN[, 1])
summary(model3.poboljsanje)

y<-model3.poboljsanje$coefficients[2]*TIME+model3.poboljsanje$coefficients[3]*TIME^2+model3.poboljsanje$coefficients[4]*cos(2*pi*TIME/8)+model3.poboljsanje$coefficients[5]*sin(2*pi*TIME/8)
plot(regr2.ts,type="l",xlab="Time")
points(TIME, y, type = "l", col="blue") #ovde se vrednost Adjusted R-squared:  0.4955 malo povecala, mada i dalje nije dovoljno dobro

#5.
install.packages("astsa")
library(astsa)
data(soi)
data(rec)
soi
rec
class(soi)
class(rec)
plot(soi)
plot(rec)
ts.plot(soi,rec) #zajednicki grafik za soi i rec
#a)
#u obe serije uocavamo sezonsku komponentu
#vidimo takodje da se ciklusi brze ponavljaju u soi nego u rec
acf(ts.union(soi,rec)) #vidimo da su zavisne jer vise od 5% upada u kriticnu oblast
ccf(rec,soi) #kros korelaciona funkcija sa negativnom zadrskom -k govori da SOI prednjaci u odnosu na REC
#uklanjamo trend i sezonsku komponentu jer je ccf definisana za stacionarne serije
soi.ran<-decompose(soi)$random #ostaje samo slucajna komponenta, izbacujemo trend i sezonsku
soi.ran #sada vidimo gde su NA vrednosti koje treba izbaciti
soi.ran.ts<-window(soi.ran,start=c(1950,7),end=c(1987,3)) 
rec.ran<-decompose(rec)$random #izbacujemo trend i sezonsku komponentu
rec.ran
rec.ran.ts<-window(rec.ran,start=c(1950,7),end=c(1987,3))
soi.ran.ts
plot(soi.ran.ts,rec.ran.ts)
acf(ts.union(soi.ran.ts,rec.ran.ts))#4 grafika, vidimo da su reziduali serije rec autokorelisani jer prelaze 5%
ccf(soi.ran.ts,rec.ran.ts)#samo kros korelacija, sa pozitivnim i negativnim koracima
cov(soi,rec)
cor(soi,rec)

#b)
#U R-u, funkcija CCF je definisana tako da odredi vezu izmedju dve serije, na primer x_(t+h) i y_t, gde je h=0,+1,-1,+2,-2,...
#ngativna vrednost h je korelacija izmedju x u nekom trenutku pre t, i y u bas tom trenutku t
#Mozemo jos i da kazemo, da, ako je h negativno tada x vodi y, a za ha pozitivno kazemo da x zaostaje za y
#pravimo model linearne regresije
vreme<-time(soi) #funkcija time izdvaja sve vremenske trenutke
sezone<-cycle(soi) #funkcija cycle...
plot(vreme)
plot(sezone)
soi.lm<-lm(soi~0+vreme+factor(sezone),data=soi) #potrebno je eksplicitno navesti nulu da u modelu ne bi postojao slobodan clan
coef(soi.lm) #prikazujemo koeficijente
#Sada cemo vrsiti predvidjanje
#prvo kreiramo vremenske trenutke koji odgovaraju mesecima pocev od 1987. godine
new.time<-seq(1987,len=2*12,by=1/12)[10:24]
new.time
new.data<-data.frame(vreme=new.time, sezone=c(10:12,1:12))
prognoza1<-predict(soi.lm,new.data)
prognoza1
#slicno se radi i za rec
rec.lm<-lm(rec~0+vreme+factor(sezone))
prognoza2<-predict(rec.lm,new.data)
prognoza2
plot(rnorm(634),type="l")

#6.
library(astsa)
data("varve")
varve
plot(varve)
#a)
#treba da proverimo stacionarnost serije, to mozemo videti sa korelograma
avf(varve) #kako kod stacionarnih serija  ACF relativno brzo tezi nuli, a kod nas to nije slucaj, ne mozemo pretpostaviti stacionarnost nase serije
#b)
#sada cemo transformisati vremensku seriju logaritamskom transformacijom jer se njom disperzija ujednaci tokom vremena
layout(1:2)
plot(varve, main="varve", ylab="")
plot(log(varve), main="log(varve)", ylab="" )
#c)
#sada cemo nacrtati korelogram logaritmovane serije
acf(log(varve)) #vidimo da se ACF nije nesto narocito promenila posle logaritmovanja
#sto znaci da treba da izvrsimo neku drugu transformaciju, a to je diferenciranje
#diferenciranjem cemo ukloniti trend i sezonsku komponentu
diff(log(varve))
plot(diff(log(varve)))
#sada crtamo korelogram
acf2(diff(log(varve)))
#Posto vidimo da ACF ima jednu znacajnu vrednost u 1, a PACF opada eksponencijalno
#zakljucujemo da je rec o modelu pokretnih proseka reda 1, tj. MA(1).
#d)
#ostalo je jos da ocenimo parametre MA(1) modela
#na osnovu kovarijacione cemo dobiti
acf(diff(log(varve)))$acf[2]
#sto nam daje vrednost -0.3974306
#na osnovu kov.f.j-e racunamo ocenu za sigma^2
acf(diff(log(varve)), type="covariance")$acf[2]
#gama_1=-0.1318, ubacimo u jednacniu -teta_1*sigma^2=gama_1....dobijemo
#sigma_1^2= 0.0652, sigma_^2 = 0.2689


