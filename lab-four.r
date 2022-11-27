library("markovchain")
t_matrix=c(
0,0,0.3,0,0,0,0,0.7,0,
0.8,0.2,0,0,0,0,0,0,0,
0,0,0,0,0,0.5,0.5,0,0,
0,0,0,0.3,0.7,0,0,0,0,
0,0.5,0,0.5,0,0,0,0,0,
0,0,0,0,0,0.6,0,0,0.4,
0.4,0,0.35,0,0,0,0,0.25,0,
0,0,0,0.85,0.15,0,0,0,0,
0,0,0,0,0,0,0.45,0,0.55)
#задать цепь
statesNames=c("a","b","c","d","e","f","g","h","i")
true_matrix=matrix(t_matrix, nrow=9,
byrow=TRUE, dimnames=list(statesNames, statesNames))
mcA<-new("markovchain", states=statesNames,
transitionMatrix=true_matrix)
mcA

library(diagram)
plot(mcA, package = "diagram", box.size = 0.06, 
main = "Граф состояний Марковской цепи")

time=seq(0,15,1)
p1=(1)
p2=(0)
p3=(0)
p4=(0)
p5=(0)
p6=(0)
p7=(0)
p8=(0)
p9=(0)
n_=c(1,0,0,0,0,0,0,0,0)
for (i in 1:15){
n_i=n_*mcA^i
c=cat(n_i[1]," ",n_i[2]," ",n_i[3]," ",n_i[4]," ",n_i[5]," ",n_i[6]," ",n_i[7]," ",n_i[8]," ",n_i[9])
print(c)
p1=c(p1,n_i[1])
p2=c(p2,n_i[2])
p3=c(p3,n_i[3])
p4=c(p4,n_i[4])
p5=c(p5,n_i[5])
p6=c(p6,n_i[6])
p7=c(p7,n_i[7])
p8=c(p8,n_i[8])
p9=c(p9,n_i[9])
}

plot(time,p1,ylim=range(c(0,1)), type="l",col="red")
lines(time,p2,col="green")
lines(time,p3,col="blue")
lines(time,p4,col="yellow")
lines(time,p5,col="pink")
lines(time,p6,col="black")
lines(time,p7,col="gray")
lines(time,p8,col="brown")
lines(time,p9,col="orange")

steadyStates(mcA)
is.regular(mcA)

data=markovchainSequence(n=50, markovchain=mcA, include=TRUE)
data

#оценка матрицы
estimated=markovchainFit(
data,
method = "map",
byrow = TRUE,
nboot = 10L,
laplacian = 0,
name = "",
parallel = FALSE,
confidencelevel = 0.99,
confint = FALSE,
hyperparam = matrix(),
sanitize = FALSE,
possibleStates = character()
)##===============
h=estimated$estimate
h

est_matrix=NULL
for (i in 1:9){
for (j in 1:9){
est_matrix=c(est_matrix,h[i,j])
}
}
est_matrix

sum=0
for (i in 1:81){
sum=sum+(t_matrix[i]-est_matrix[i])^2
}
sum
q=sqrt(sum)
q

q_=q
n_=c(100,200,500,1000)
for (n in n_) {
#моделирование
len=n
data=markovchainSequence(n=len, markovchain=mcA, include=TRUE)
data
#оценка матрицы
estimated=markovchainFit(
data,
method = "map",
byrow = TRUE,
nboot = 10L,

laplacian = 0,
name = "",
parallel = FALSE,
confidencelevel = 0.99,
confint = FALSE,
hyperparam = matrix(),
sanitize = FALSE,
possibleStates = character()
)##===============
h=estimated$estimate
h
est_matrix=NULL
for (i in 1:9){
for (j in 1:9){
est_matrix=c(est_matrix,h[i,j])
}
}
sum=0
for (i in 1:81){
sum=sum+(t_matrix[i]-est_matrix[i])^2
}
sum
q_=c(q_,sqrt(sum))
}
n_=c(50,n_)
n_
q_
plot(n_,q_, type="l",col="red")