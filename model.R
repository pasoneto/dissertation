library(data.table)
library(stringr)
library(dplyr)
library(stargazer)
library(gridExtra)
library(xlsx)
library(apsrtable)
library(readxl)

#Set working directory
setwd("C:/Users/Lenovo/Desktop/world/Ciência/UFABC/Dissertação/scripts") 

#Read data set
data = 
  fread('exp1.csv', index = NULL) 

#Model overall
overall_m =
  lm(response ~
       
       #scale_step+
       cent+
       scale_step:factor(condition)+
       cent:factor(condition),
       
     data = data
     )
summary(overall_m)

#Non-normal distribution of residuals
#hist(overall_m$residuals)
#shapiro.test(overall_m$residuals)

####################################
## Normalizing dependent variable ##
####################################

#empty column to receive z
data$z_score <- 0

#Splits df by participant
data = 
  split(data, data$participant_id)


#Computes z for eack participant, separatelly
for(i in 1:length(data)){
  media = mean(data[[i]]$response, na.rm = T)
  sd = sd(data[[i]]$response, na.rm = T)
  data[[i]]$z_score = ( (data[[i]]$response - media) / sd )
}

#Reunites data frame
data =
  bind_rows(data)


#Model normal
overall_normal =
  lm(z_score ~
       
       #scale_step+
       cent+
       scale_step:factor(condition)+
       cent:factor(condition),
     
     data = data
  )
summary(overall_normal)


#Note: residuals are still non-normal:
shapiro.test(overall_normal$residuals)





#########################
## SIMULATIONS SECTION ##
#########################

#Function to simulate intervallic perception
#(Parameters described in the final model of the dissertation)
interval_finding = 
  function(c, sc, t){
    return(7.19 + ((0.063*c) + (-0.017*c*t))  + ((1.62*sc*t) + (2.28*sc*t)))
  }

#Plotting experimental data against simulated data
plot<- plyr::ddply(data, c('condition', 'interval'), summarise,
                   N    = length(response),
                   mean = mean(response, na.rm = TRUE),
                   sd   = sd(response, na.rm = TRUE),
                   se = sd / sqrt(N)
)

plot$cent <- c(300, 300, 300, 400, 400, 400, 400)
plot$scale_step <- c(2, 2, 2, 2, 2, 2, 3)

#Calculating intervallic distances based on the model
#described in the dissertation

distance = c()
for(i in 1:length(plot$condition)){
  distance = c(distance, 
               #Function to calculate intervallic distances (defined above)
               interval_finding(c = plot$cent[i], 
                                sc = plot$scale_step[i], 
                                t = plot$condition[i]))
}
plot$simu <- distance

#Plotting
exp <- ggplot(plot, aes(x=as.factor(interval), y=mean, colour = as.factor(condition))) +
  geom_errorbar(size = 0.9, aes(ymin=mean-se, ymax=mean+se), width=.5, position=position_dodge(0.5)) +
  geom_point(shape = 21, size = 2.5, position=position_dodge(0.5), fill = 'white')+
  theme(legend.title = element_blank(), legend.justification = "left")+
  theme(legend.position="bottom")+
  guides(col=guide_legend("Condition"))+
  ylab('Average estimated distance')+
  xlab('Intervals')+
  ylim(c(25, 45))

simu <-  ggplot(data = plot, aes(x = as.factor(interval), 
                                 y = simu,
                                 group = as.factor(condition),
                                 colour = as.factor(condition)
                                 ))+
              ylab('Simulated distances')+
              xlab('Intervals')+
              theme(legend.position="bottom")+
              guides(col=guide_legend("Condition"))+
              geom_line(linetype = 2, size = 1)+
              ylim(c(25, 45))

final = grid.arrange(exp, simu, nrow = 1)

final

#####################
###  EXPERIMENT 2  ##
#####################

#Estimating intervallic distances based on the same model as before
t_minor = interval_finding(c = 300, sc = 2, t = 1)
t_major = interval_finding(c = 400, sc = 2, t = 1)
at_minor = interval_finding(c = 300, sc = 2, t = 0)
at_major = interval_finding(c = 400, sc = 2, t = 0)

#Calculating intervallic differences
dif_t = abs(t_minor - t_major)
dif_at = abs(at_minor - at_major)

#Reading data from exp 2
dados = 
  read_excel("exp2.xlsx")

#Filtering out participants as described in the dissertation
dados <- filter(dados, 
                filtro_overall == 0)

#Selecting ariables of interest
graficos <- data.frame(participants = c(dados$participants),
                       d_atonal = dados$d_atonal,
                       d_tonal = dados$d_tonal,
                       Training = dados$Training)

#Changing from wide to long format
graficos <- melt(data = graficos, d_prime = c("d_atonal", 
                                              "d_tonal"), 
                        id.var = c("Training",
                                   "participants"))

#Adding empti column to receive simulated distance values
graficos$distance = 0

#Adding values to mach condition (0 = atonal; 1 = tonal)
graficos$distance[1:368]= dif_at
graficos$distance[369:nrow(graficos)]= dif_t

#Model 2

model2 = 
  lm(data = graficos, 
     formula = value~distance)
summary(model2)

#Results non-normal again. 
#d'primes are already normal. Don't know what to do here.
hist(model2$residuals)
shapiro.test(model2$residuals)


#Plotting model 2
plot<- plyr::ddply(graficos, c('variable', 'distance'), summarise,
                   N    = length(value),
                   mean = mean(value, na.rm = TRUE),
                   sd   = sd(value, na.rm = TRUE),
                   se = sd/sqrt(N)
)

distance = 
  ggplot(data = plot, aes(x = variable, y = distance, group = 1))+
    geom_line(linetype = 2, size = 0.5)+
    ylab('Distance')

d_prime = 
  ggplot(data = plot, aes(x = variable, y = mean, group = 1))+
    geom_line(size = 0.5)+
    geom_errorbar(size = 0.5, aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(0.5)) +
    geom_point(shape = 21, size = 2.5, position=position_dodge(0.5), fill = 'white')+
    ylab('d-prime')

final2 = grid.arrange(distance, d_prime, nrow = 1)

final2


#Saves graphs in HD
#ggsave(final, filename = "C:/Users/Lenovo/Desktop/world/Ciência/UFABC/Dissertação/images/fig15.png", dpi = 600,
#       width = 6, height = 4.5, units = "in")

#ggsave(final2, filename = "C:/Users/Lenovo/Desktop/world/Ciência/UFABC/Dissertação/images/fig15.png", dpi = 600,
#       width = 6, height = 4.5, units = "in")

