
#----start----
rm(list = ls())
library(ggplot2)

generation = 1 # starting generation is 0
generation_end = 200 # to define the duration of the generation

replicates=25
all_data = matrix(ncol = 8)
encounter_probabilities = c( 0.1,0.5,1)  
#encounter_probabilities = c(0.25, 0.5)
B_increase_peregg = c(0.1, 0.25, 0.5)
#B_increase_peregg = c(0.25, 0.5)
death_probablity = c(0.1, 0.25, 0.5, 0.75)
opportunity_vec = c(4, 12, 24)
idx = 1


for (encount in encounter_probabilities){
  
  for (B_increase_1egg in B_increase_peregg) {  
    
    for (death_prob in death_probablity) {
      
      for (mating_opportunities in opportunity_vec) { 
        
        
        track2<-array(0,dim=c(2,(generation_end+1),replicates))
        
        for(replicate in 1:replicates) {#start replicate loop
          
          
          # Paramteres
          
          # Individuals/population Parameters ----
          
          initial_ind_type = c(1,2,3,4,5,6) # initial individuals types
          ind_types = c(1,2,3,4,5,6) # all types of individuals
          #opportunity=10
          # 1 - cooperative with eggs
          # 2 - cooperative without eggs
          # 3 - non-cooperative with eggs
          # 4 - non-cooperative without eggs
          # 5 - cooperative -: eggs senescence, have eggs, BUT NOT functioning as the female
          # 6 - Non-cooperative -: eggs senescence, have eggs, BUT NOT functioning as the female
          
          
          # each type of individual vector
          type_1_vec = rep(initial_ind_type[1],10)
          type_2_vec = rep(initial_ind_type[2],0)
          type_3_vec = rep(initial_ind_type[3],10)
          type_4_vec = rep(initial_ind_type[4],0)
          type_5_vec = c()
          type_6_vec = c()
          
          # population vector, to track every individual. Every individual has its definit position, to track the individual
          pop_vec = c(type_1_vec,type_2_vec,type_3_vec,type_4_vec,type_5_vec,type_6_vec)
          pop_size = length(pop_vec)
          
          E_initial_num = 20 # initial number of eggs individuals have
          E_initial_vec = c(rep(E_initial_num,length(type_1_vec)),rep(0,length(type_2_vec)),rep(E_initial_num,length(type_3_vec)),rep(0,length(type_1_vec))) # Initial egg vector
          
          #  body size parameter --------------
          
          B_min = 5 # body size can't reduce bellow this point
          B_initial_vec = rep(B_min,pop_size)
          B_critical_1 = 15 # Critical body size, egg production start after this point
          
          # bodysize_increase
          #B_increase_1egg = 0.3
          
          # Egg parameter -----------
          E_critical_1 = 20 # After this point individual start reproduction
          
          # Growth related parameters
          
          K = 200 # carrying capacity
          
          encounter_prb = encount # probability with which two individuals will encounter 
          
          eggvec = E_initial_vec # just redefining, initially all individuals have 40 eggs
          bodysize_vec = B_initial_vec # just redefining, initial body size
          
          #mating_opportunities=10
          # death_prob=.2
          
          
          #all_gen_eggMat = array(0,dim = c(2,mating_opportunities+1,generation_end))
          track=matrix(length(type_1_vec),2)
          
          for(generation in 1:generation_end){ # loop generation---------------
            
            hatching_mat = data.frame(matrix(0,nrow = 1, ncol = 2))
            colnames(hatching_mat) = c("coop_eggs","noncoop_eggs")
            
            
            for(opportunity in 1:mating_opportunities){ #loop mating opportunities-----------
              
              eggnum2=c(0,0)
              
              surviving_individuals= rbinom(length(pop_vec),1,death_prob)==0 # Boolean values for surviving individuals
              pop_vec=pop_vec[surviving_individuals] 
              bodysize_vec = bodysize_vec[surviving_individuals]
              eggvec=eggvec[surviving_individuals] 
              
              if(length(pop_vec)>1){
                rand_index_encountering=sample(1:length(pop_vec)) # randomly selecting the indexes
                encountering_pop_length=length(rand_index_encountering) # length of population who are surviving and randomized
                ii = rand_index_encountering[seq(1,encountering_pop_length,2)] # making the vector from 1 to n-2
                jj = rand_index_encountering[seq(2,encountering_pop_length,2)] # vector from 2 to n
                
                
                even_stuff_out=min(length(ii),length(jj))
                ii=ii[1:even_stuff_out]  # focal individuals selected
                jj=jj[1:even_stuff_out] # partner individual selected
                
                encounters_happening=rbinom(length(ii),1,encounter_prb)==1 # same thing as survival .... in BOOLIEN
                
                if(sum(encounters_happening)>0){
                  
                  ii=ii[encounters_happening] # focal individuals mated successfully
                  jj=jj[encounters_happening] # partner individuals mated successfully
                  
                  x_checker=function(x,compareto){ 
                    
                    return(sum(x==compareto)==2) #|sum(rev(x)==compareto)==2) 
                  }
                  
                  # for mating cobminations, and the egg type result for next generation
                  laying_eggs=t(apply(cbind(pop_vec[ii],pop_vec[jj]),1,FUN=function(x){ # unction not uderstood vel, in apply 
                    # t - for transpose
                    # cbind - it is connecting vector to form a matrix
                    
                    r=c(0,0) #does this individual lay eggs
                    coop_prob=0 #probability for offspring from this mating to be cooperators
                    
                    if(x_checker(x,c(1,1))) # what is ==2 means
                    {
                      r[1:2] = 1
                      coop_prob = 1
                      
                      # Individual type 1 and 2,5,7
                      # need to add change in individual type function
                    } else if(x_checker(x,c(1,2)) || x_checker(x,c(1,5)) ){#if we have a meeting between 1 &2 | x==c(1,2)|x==c(2,1)
                      r[x==1]=1
                      coop_prob=1
                      
                      # t
                    } else if(x_checker(x,c(1,3))){
                      r[1:2]=1
                      coop_prob=.5
                      
                    } else if (x_checker(x,c(1,4)) || x_checker(x,c(1,6))){
                      r[x=1] = 1
                      coop_prob = .5
                      
                      
                      # type 2 individual
                    } else if (x_checker(x,c(2,1))) { 
                      r[x=2] = 1
                      coop_prob = 1
                      
                    } else if (x_checker(x,c(2,2)) || x_checker(x,c(2,3)) || x_checker(x,c(2,4)) || x_checker(x,c(2,5)) || x_checker(x,c(2,6)) ) {
                      r[x=2] = 0
                      coop_prob = 0
                      
                      # type 3 individual
                    } else if (x_checker(x,c(3,1))) {
                      r[1:2] = 1
                      coop_prob = .5
                    } else if (x_checker(x,c(3,2)) || x_checker(x,c(3,4)) || x_checker(x,c(3,5)) || x_checker(x,c(3,6)) ) {
                      r[x=2] = 0
                      coop_prob = 0
                      
                    } else if (x_checker(x,c(3,3))) {
                      r[1:2] = 1
                      coop_prob = 0
                      
                      # type 4 individual
                    } else if (x_checker(x,c(4,1))) {
                      r[x=2] = 1
                      coop_prob = .5
                      
                    } else if (x_checker(x,c(4,2)) || x_checker(x,c(4,3)) || x_checker(x,c(4,4)) || x_checker(x,c(4,5)) || x_checker(x,c(4,6)) ) {
                      r[x=2] = 0
                      coop_prob = 0
                      
                      # type 5 individual
                    } else if (x_checker(x,c(5,1))) {
                      r[x=2] = 1
                      coop_prob = 1
                    } else if (x_checker(x,c(5,2)) || x_checker(x,c(5,3)) || x_checker(x,c(5,4)) || x_checker(x,c(5,5)) || x_checker(x,c(5,6)) ) {
                      r[x=2] = 0
                      coop_prob = 0
                      
                      # type 6 individual
                    } else if (x_checker(x,c(6,1))) {
                      r[x=2] = 1
                      coop_prob = .5
                    } else if (x_checker(x,c(6,2)) || x_checker(x,c(6,3)) || x_checker(x,c(6,4)) || x_checker(x,c(6,5)) || x_checker(x,c(6,6)) ) {
                      r[x=2] = 0
                      coop_prob = 0
                    }
                    return(c(r,coop_prob))
                  }))
                  
                  
                  
                  # Some times there is black table, why? 
                  if (length(laying_eggs)>0){
                    colnames(laying_eggs)=c("mating intividial 1 mating?", "mating intividial 2 mating?","prob offspring being cooperators")
                    rownames(laying_eggs)=paste("mating pair",1:nrow(laying_eggs) )  
                  }
                  
                  # egg layer individuals
                  egg_layers=cbind(ii,jj)[laying_eggs[,-3]==1] # individuals index position, who are going to lay the eggs, except col 3 in egg layers individuals who will lay the eggs 1
                  
                  # Index positions of not egg layers
                  if (length(egg_layers)!=0){
                    not_egg_layers=(1:length(pop_vec))[-egg_layers]
                  } else if(length(egg_layers)==0) {
                    not_egg_layers = 1:length(pop_vec)
                  }
                  
                  
                  number_of_eggs_per_pair=rowSums(as.matrix(eggvec[cbind(ii,jj)]*laying_eggs[,-3],ncol=2)) # rowSum because both individuals are laying eggs
                  
                  
                  # Total number of eggs
                  numeggs=c(sum(number_of_eggs_per_pair[laying_eggs[,3]==1]),sum(number_of_eggs_per_pair[laying_eggs[,3]==0]), sum(number_of_eggs_per_pair[laying_eggs[,3]==.5]))
                  chancis=rbinom(1,numeggs[3],.5) # for mating of different type of individuals
                  numeggs2=c(numeggs[1]+chancis,numeggs[2]+(numeggs[3]-chancis))
                  eggnum2=eggnum2+numeggs2
                  new_coop_noncoop=c(eggnum2[1],eggnum2[2]) # total cooperative and non-cooperative eggs
                  #print(new_coop_noncoop)
                  
                  # number of eggs produced in current round are added in the matrix 
                  #hatching_mat[opportunity,] = c(new_coop_noncoop,0) 
                  ##
                  hatching_mat = rbind(hatching_mat,c(new_coop_noncoop))
                  ##
                  #hatching_mat[is.na(hatching_mat)] = 0 # some times there are NA values, don't know why
                  
                  # successfull individual will have minimum body size and egg will be 0  
                  bodysize_vec[egg_layers] = B_min
                  eggvec[egg_layers] = 0 # eggvec is for the individuals having the eggs ...remeber this
                  
                  # Index positions of not egg layers based on individual types and body size
                  not_egg_layers_2_4_large =  not_egg_layers[(pop_vec[not_egg_layers]==2 | pop_vec[not_egg_layers]==4) & (bodysize_vec[not_egg_layers] >= B_critical_1)]
                  not_egg_layers_2_4_small = not_egg_layers[(pop_vec[not_egg_layers]==2 | pop_vec[not_egg_layers]==4) & (bodysize_vec[not_egg_layers] < B_critical_1)]
                  not_egg_layers_5_6 = not_egg_layers[pop_vec[not_egg_layers]==5 | pop_vec[not_egg_layers]==6]
                  not_egg_layers_1_3 = not_egg_layers[pop_vec[not_egg_layers]==1 | pop_vec[not_egg_layers]==3]
                  
                  
                  # All egg layers will change to type 2/4
                  pop_vec[egg_layers][pop_vec[egg_layers]==1] = 2
                  pop_vec[egg_layers][pop_vec[egg_layers]==3] = 4
                  
                  # type 2/4 with larger body size, produce the eggs, decrease the body size, type will change to 1 OR 3
                  
                  decrease_in_B_size_2_4 = bodysize_vec[not_egg_layers_2_4_large] - B_min
                  eggvec[not_egg_layers_2_4_large] = eggvec[not_egg_layers_2_4_large] + round(decrease_in_B_size_2_4 * 2) # change 2 to number of eggs produced decrease in unit body size
                  bodysize_vec[not_egg_layers_2_4_large] = B_min
                  pop_vec[not_egg_layers_2_4_large][pop_vec[not_egg_layers_2_4_large]==2 & eggvec[not_egg_layers_2_4_large]>0] = 1 
                  pop_vec[not_egg_layers_2_4_large][pop_vec[not_egg_layers_2_4_large]==4 & eggvec[not_egg_layers_2_4_large]>0] = 3
                  
                  # type 2 OR 4 with smaller body size, will increase the body size 
                  bodysize_vec[not_egg_layers_2_4_small] = bodysize_vec[not_egg_layers_2_4_small] + 10 
                  
                  
                  # Individuals who are type 5 or 6, relatively larger body size than critical body size
                  decrease_in_B_size_5_6 = bodysize_vec[not_egg_layers_5_6] - B_min
                  eggvec[not_egg_layers_5_6] = eggvec[not_egg_layers_5_6] + round(decrease_in_B_size_5_6*2)
                  bodysize_vec[not_egg_layers_5_6] = B_min
                  pop_vec[not_egg_layers_5_6][pop_vec[not_egg_layers_5_6]==5 & eggvec[not_egg_layers_5_6]>0] = 1
                  pop_vec[not_egg_layers_5_6][pop_vec[not_egg_layers_5_6]==6 & eggvec[not_egg_layers_5_6]>0] = 3
                  
                  
                  # Unsuccessful individuals Type 1 & 3, eggs digest, increase body size, type change to type 5 OR 6
                  
                  eggs_digested = eggvec[not_egg_layers_1_3]
                  eggvec[not_egg_layers_1_3] = eggvec[not_egg_layers_1_3] - eggs_digested
                  bodysize_vec[not_egg_layers_1_3] = bodysize_vec[not_egg_layers_1_3] + (eggs_digested *  B_increase_1egg) + 10 # change 10 to increase in body size with unit time
                  pop_vec[not_egg_layers_1_3][pop_vec[not_egg_layers_1_3]==1 & eggvec[not_egg_layers_1_3] == 0] = 5 
                  pop_vec[not_egg_layers_1_3][pop_vec[not_egg_layers_1_3]==3 & eggvec[not_egg_layers_1_3] == 0] = 6
                  
                }#end of "if encounter happen"
                
              }#end of "if there are more than 1 individual
              
            }#end of opportunity------------
            
            # All generation egg vec
            
            #all_gen_eggMat[,opportunity,generation] = t(hatching_mat) 
            # Individuals in next generation
            new_ind_coop = sum(hatching_mat[,1])
            new_ind_noncoop = sum(hatching_mat[,2])
            
            # redefining the vectors
            pop_vec<-c()
            bodysize_vec<-c()
            eggvec<-c()
            
            # Applying population dependent mortality
            egg_survival<-min(max(K/sum(new_ind_coop,new_ind_noncoop),0),1)
            
            
            new_ind_coop2<-rbinom(1,new_ind_coop,egg_survival)
            if (length(new_ind_coop2) > 0 ) {
              pop_vec = c(pop_vec,rep(2,new_ind_coop2))
              bodysize_vec = c(bodysize_vec,rep(B_min,new_ind_coop2))
              eggvec = c (eggvec,rep(0,new_ind_coop2))
            }
            
            new_ind_noncoop2<-rbinom(1,new_ind_noncoop,egg_survival)
            if (length(new_ind_noncoop2) > 0 ) {
              pop_vec = c(pop_vec,rep(4,new_ind_noncoop2))
              bodysize_vec = c(bodysize_vec,rep(B_min,new_ind_noncoop2))
              eggvec = c (eggvec,rep(0,new_ind_noncoop2))
            }
            
            track= cbind(track,table(factor(pop_vec,c(2,4))))
            
            # tracking population dynamics over the generations
          } # end of generation ----------------
          
          track2[,,replicate]<-track
        } # end to replicates--------------
        
        #create graphs------
        
        track3 <- apply(track2,c(1,2),mean,na.rm=T)
        track4 <- apply(track2,c(1,2),sd,na.rm=T)
        
        
        EP = paste("E", encount,sep = ".")
        BperE = paste("B",B_increase_1egg,sep = ".")
        DP = paste("D",death_prob,sep = ".")
        MP = paste("M",mating_opportunities,sep = "")
        conditions = paste(EP,BperE,DP,MP,sep = "_")
        
        time <- seq(1:length(track3[1,]))
        ylim1 = max(track3) + max(track4)
        ylim2 = min(track3) - max(track4)
        # assign(paste0("track_",encount,"_",B_increase_1egg,"_",death_prob,"_",mating_opportunities),track3)
        # Create the directory
        
        # to save the graph
      
        pop_sizemat = matrix(c(encount, B_increase_1egg, death_prob,mating_opportunities),nrow = length(t(track3[1,])),ncol = 4,byrow = TRUE)
        pop_sizemat = cbind(pop_sizemat,c(track3[1,]),(track4[1,]),(track3[2,]),(track4[2,]))
        all_data = rbind(all_data,pop_sizemat)
        #pop_sizemat = rbind(pop_sizemat,encount,B_increase_1egg,death_prob,mating_opportunities)
        
        cooperators_ind = pop_sizemat[,5]
        noncooperators_ind = pop_sizemat[,7]
        
        #matplot(time,cooperators_ind,col = "red",type = "b",)
        
        
        
        
        data_plot = ggplot(data.frame(pop_sizemat),aes(time,cooperators_ind)) + geom_point(colour = c('red')) + geom_errorbar(aes( ymin= cooperators_ind - pop_sizemat[,6], ymax= cooperators_ind + pop_sizemat[,6])) + 
          geom_point(aes(time,noncooperators_ind),colour = 'blue') + geom_errorbar(aes( ymin= noncooperators_ind - pop_sizemat[,8], ymax= noncooperators_ind + pop_sizemat[,8] )) + labs(y = "population size")+ ggtitle(conditions) 
        
        png(file = paste("B:/research_modulr_graph/",idx,conditions,'.png',sep = ""))
        print(data_plot)
        dev.off()
        idx = idx + 1
        data_plot
      } # mating_opportunities
    } # Death_prob
  } # B_increase
} # encount

all_data = all_data[-1,]
colnames(all_data) = c("encounter_prob","size_increase_per_egg","death_prob","mating_opportunities_num","coop_mean","coop_sd","noncoop_mean","noncoop_sd" )
write.csv(all_data,"B:/research_modulr_graph/all_data.csv")
