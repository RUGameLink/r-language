T=1000
TAU=50 #для АКФ
LEN_B=20
array=rweibull(T+LEN_B, shape=1, scale = 1)

#функция АКФ
ACT_FUN = function(e, i){
   result <- c() #Объявляем массив методом с()
   mean_e = mean(e) #Средняя
   var_e = var(e) #Дисперсия
   if(i == 1){ #Проверяем какой размерности пришел массив с помощью передаваемой переменной i
      size = T + LEN_B
   }
   else{
      size = T
   }
   for(t1 in 0:TAU){ #Реализация АКФ функции
      sum = 0
      for(t2 in 1:(size  - t1)){
            sum = sum + ((e[t2] - mean_e) * (e[t2 + t1] - mean_e)) #Реализуется числитель формулы
         }
      result[(t1+1)] = (sum / (((size) - 1 - t1) * var_e)) #Результирующая для текущего элемента (со второго элемента)
   }
   return(result);
}
#PR-метод
PR_FACT = function(e){
buffer <- c()
result <- c()

for(i in 1:LEN_B){
 buffer[i] = e[i + 1]
}
result[1] = e[1]
for(i in 2:T){
 min = 100
 value = 0
 index = 0
 for(j in 1:LEN_B){
  subst = result[i - 1]
  subs = buffer[j]
  sub = abs(subst - subs)
  if(sub < min){
   index = j
   min = sub
   value = buffer[j]
  }
 }
result[i] = value
buffer[index] = e[i + LEN_B]
}
return(result)
}
#Начало программы
array
print("Хи-квадрат до перестановки")
k<-50     # число интервалов
int<-seq(min(array),max(array),(max(array)-min(array))/k) #интервалы для расчета частот
array.fact<-hist(array,breaks=int,plot=FALSE)
print("Хи-квадрат")
int[1]<-(0)
int[k+1]<-(Inf)			  #границы
array.theor<-pweibull(int,shape=1,scale=1)


array.theor<-(array.theor[2:(k+1)]-array.theor[1:k])
chisq.test(array.fact$counts,p=array.theor, simulate.p.value=TRUE)
#Значения до PR
print("Статические характеристики до перестановки")
m_array=mean(array)#Среднее
m_array 
disp=var(array)#Дисперсия
disp
sqo=sd(array)#Среднее квадратичное
sqo
par(mfrow = c(1, 2))
p1<-hist(array, breaks = 30, freq = FALSE, col = "lightgreen")
lines(density(array),col = "red") #таблица и график выборочных частот
p1

p1$counts <- cumsum(p1$counts) 
plot(p1,col = "red") #таблица и график теоретических частот
p1

print("PR-метод")
pr <- PR_FACT(array) #Все тоже самое, но для PR метода
pr
print("Хи-квадрат после перестановки")
k<-50     # число интервалов
int<-seq(min(pr),max(pr),(max(pr)-min(pr))/k) #интервалы для расчета частот
pr.fact<-hist(pr,breaks=int,plot=FALSE)
int[1]<-(0)
int[k+1]<-(Inf)			  #границы
pr.theor<-pweibull(int,shape=1,scale=1)

pr.theor<-(pr.theor[2:(k+1)]-pr.theor[1:k])
chisq.test(pr.fact$counts,p=pr.theor, simulate.p.value=TRUE)
print("Статические характеристики после перестановки")
m_pr=mean(pr)
m_pr
disp_pr=var(pr)
disp_pr
sqo_pr=sd(pr)
sqo_pr
#Значение после PR
par(mfrow = c(1, 2))
p1<-hist(pr, breaks = 30, freq = FALSE, col = "lightgreen")
lines(density(pr),col = "red") #таблица и график выборочных частот
p1

p1$counts <- cumsum(p1$counts) 
plot(p1,col = "red") #таблица и график теоретических частот
p1

print("Статические характеристики")
m_array=mean(array)
m_array
disp=var(array)
print(disp)
sqo=sd(array)
print(sqo)


print("АКФ автоматически для исходного")
y=acf(array,lag.max = TAU,plot=FALSE) #АКФ автоматом
y
#АКФ руками
print("АКФ вручную")
acf1<-ACT_FUN(array,1)
acf1

#График до перестановки
par(mfrow=c(4,1))
plot(array, type="l")
y=acf(array,lag.max = TAU,plot=FALSE)
y
plot(y,type="l",col="red")
#График после перестановки
plot(pr,type="l")
y=acf(pr,lag.max = TAU,plot=FALSE)
y
plot(y,type="l",col="red")
print("АКФ автоматически для перестановки")
acf2=acf(pr,lag.max = TAU,plot=FALSE) #АКФ автоматом
acf2
#АКФ руками
print("АКФ вручную для перестановки")
acf3<-ACT_FUN(pr, 2)
acf3
