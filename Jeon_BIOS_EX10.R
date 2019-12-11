# Setup: 
#  when cancer drug is absent, Mutation does not affect on growth rate = 0.1
#  when cancer drug is present, Mutant subgroup grows at 50% of its growth rate in the abscence of drug = 0.05
#  when cancer drug is present, Non-mutant subgroup grow at -0.1 growth rate
#  Original cell count : M0= 1 N0=99
r_n = -0.1
r_m = 0.1/2 
K = 10^6
N0 = 99
M0 = 1
timesteps= 90

# create vector to store N's and set initial N
N=numeric(length=timesteps)
M=numeric(length=timesteps)

N[1]=N0
M[1]=M0

# simulate
for(t in 1:(timesteps-1)) {
  N[t+1] = N[t] + r_n * N[t]* (1-(N[t]+M[t])/K)
  M[t+1] = M[t] + r_m * M[t]* (1-(N[t]+M[t])/K)
}

library(ggplot2)
pop <- data.frame(time=1:length(N), Num_N=N, Num_M=M)

ggplot(data=pop) + geom_line(aes(x=time, y=Num_N), col="red")+ geom_line(aes(x=time, y=Num_M), col="blue")+
  ylab("Number of cells") + xlab("Time") 

