load("df_complete.Rdata")
plans = unique(df.complete$plan)
library(R2jags)
#library(nnet)
#summary(multinom(review ~ rent + distance, data = df.complete))
get_df = function(pl){
  df = df.complete%>%filter(plan == pl)

  
  cat("data{
      for(i.aprt in 1:n.aprt){ # this i.aprt represents the number of apartments
      C0[i.aprt,1] ~ dnorm(0,0.1)
      C0[i.aprt,2] ~ dnorm(0,0.1)
      C0[i.aprt,3] ~ dnorm(0,0.1)
      C0[i.aprt,4] ~ dnorm(0,0.1)
      C0[i.aprt,5] ~ dnorm(0,0.1)
      }
      }model{
      # likelihood
      for(i in 1:n){# This i represents # of reviews for all apartments
      Y[i] ~ dcat(P[i,])
      # probability of taking a value in one of the categories
      # separated by 5 cuts (we have six ordered categories 0 to 5 star)
      # so P[i,]: dim 1*6
      P[i,1] <- max(min(1 - Q[i,1],1),0)
      for (i.cut in 2:n.cut){
      P[i,i.cut] <- Q[i,i.cut-1] - Q[i,i.cut]
      }
      P[i,n.cut+1] <- max(min(Q[i,n.cut],1),0)
      # random effect
      for(i.cut in 1:n.cut){ # Z[i,]: dim 1*5
      logit(Q[i,i.cut]) <- Z[i,i.cut]
      Z[i,i.cut] <- b1*distance[i] + b2*rent[i] - C[(aprt[i]),i.cut] 
      }
      }
      # priors
      b1 ~ dnorm(0.0,0.01)
      b2 ~ dnorm(0,0.01)
      for(i.aprt in 1:n.aprt){ # C[i.aprt,] : dim 1*5
      C[i.aprt,1:5] <- sort(C0[i.aprt,])
      }
      }", fill=TRUE, file="reorderedlogit.txt")
  #unload.module("glm")
  jags_data = list(Y =as.numeric(df$review)+1, distance =as.numeric(scale(df$distance)),
                   rent = as.numeric(scale(df$rent)),n.cut = (length(unique(df$review))-1), 
                   n = nrow(df),n.aprt = length(unique(df$name)), aprt = as.numeric(as.factor(df$name)))
  
  params = c("P","b1","b2","C")
  
  ni = 10000; nb = 1000; nt = 20; nc = 3
  
  outj = jags(jags_data,parameters=params, model.file="reorderedlogit.txt", 
              n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
  
  label_prob = outj$BUGSoutput$sims.matrix[,grepl("P",colnames(outj$BUGSoutput$sims.matrix))]
  #save(label_prob, file="labels.Rdata")
  
  classprb = lapply(0:5,function(i) label_prob[,cumsum(table(factor(df$name, levels=unique(df$name))))+nrow(df)*i])
  for (i in seq_along(classprb)){
    colnames(classprb[[i]])  = unique(df$name)
  }
  save(classprb, file=paste0("classprb",gsub(" ","",pl),".Rdata"))
}
sapply(plans,get_df)

#aaa=sapply(classprb,function(x) apply(x,2,function(i) quantile(i,0.3)))
#colnames(aaa) = c("0 stars","1 stars","2 stars","3 stars","4 stars","5 stars")
#sort(apply(aaa,1,function(x) weighted.mean(0:5,x)))