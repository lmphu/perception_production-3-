library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(yarrr)
library(car)
library(report)
library(emmeans)
library(wesanderson)
library(data.table)
library(robustlmm)
library(lmtest)

#import production files
Control_d <- read.csv("~/Desktop/Dissertation/Percprod_data/Control_d.csv", sep=";", stringsAsFactors=TRUE)
control_t <- read.csv("~/Desktop/Dissertation/Percprod_data/control_t.csv", sep=";", stringsAsFactors=TRUE)
inter_t <- read.csv("~/Desktop/Dissertation/Percprod_data/inter_t.csv", sep=";", stringsAsFactors=TRUE)
interaction_d <- read.csv("~/Desktop/Dissertation/Percprod_data/interaction_d.csv", sep=";", stringsAsFactors=TRUE)

#these are the files with the speech rate annotattions
interaction_d <- read.csv("~/Desktop/Dissertation/Percprod_data/inter_d_SR.csv", sep=";", stringsAsFactors=TRUE)
inter_t <- read.csv("~/Desktop/Dissertation/Percprod_data/inter_t_SR.csv", sep=";", stringsAsFactors=TRUE)
Control_d <- read.csv("~/Desktop/Dissertation/Percprod_data/Contr_d_SR.csv", sep=";", stringsAsFactors=TRUE)
control_t <- read.csv("~/Desktop/Dissertation/Percprod_data/contr_t_SR.csv", sep=";", stringsAsFactors=TRUE)

#import the perception files
trials_and_sessions_c_t <- read.csv("~/Desktop/Dissertation/Percprod_data/data_percprod_c_t_all1file/trials_and_sessions_c_t.csv", stringsAsFactors=TRUE)
trials_and_sessions_cd <- read.csv("~/Desktop/Dissertation/Percprod_data/data_percprod_cd_all1file/trials_and_sessions_cd.csv", stringsAsFactors=TRUE)
trials_and_sessions_d <- read.csv("~/Desktop/Dissertation/Percprod_data/data_percprod_d_all1file/trials_and_sessions_d.csv", stringsAsFactors=TRUE)
trials_and_sessions_t <- read.csv("~/Desktop/Dissertation/Percprod_data/data_percprod_t_allin1/trials_and_sessions_t.csv", stringsAsFactors=TRUE)



# first: some cleaning, let's make it useable
#add the condition in each file
contr_d <- Control_d %>% mutate(condition = "contr_d")
contr_d <- contr_d %>% filter(!(Rec_Session_Id == 618328)) #exclude participant with baby in the background who somehow made it into the data frame
contr_t <- control_t %>% mutate(condition = "contr_t")
inter_d <- interaction_d %>% mutate(condition = "inter_d")
inter_t <- inter_t %>% mutate(condition = "inter_t")


contr_d <- rename(contr_d, prodstim_pre = copy_of_copy_of_factor1_tg1_task5)
contr_d <- rename(contr_d, prodstim_post = copy_of_copy_of_copy_of_factor1_tg1_task5)
contr_d <- unite(contr_d, "prod_stim", prodstim_pre:prodstim_post, sep = "", remove = TRUE, na.rm = TRUE)

contr_d$Sounding_duration <- as.character(contr_d$Sounding_duration)
contr_d$Sounding_duration <- as.numeric(contr_d$Sounding_duration)
contr_d <- contr_d %>%
  group_by(Rec_Session_Id) %>%
  mutate(mean_SRcol = mean(Sounding_duration, na.rm = TRUE))

contr_d$Sounding_duration[is.na(contr_d$Sounding_duration)] <- contr_d$mean_SRcol[is.na(contr_d$Sounding_duration)]

contr_d <- contr_d%>% rename(Speech_duration= Sounding_duration)

contr_t <- rename(contr_t, prodstim_pre = copy_of_copy_of_factor1_tg1_task5)
contr_t <- rename(contr_t, prodstim_post = copy_of_copy_of_copy_of_factor1_tg1_task5)
contr_t <- unite(contr_t, "prod_stim", prodstim_pre:prodstim_post, sep = "", remove = TRUE, na.rm = TRUE)

contr_t$sounding_duration <- as.character(contr_t$sounding_duration)
contr_t$sounding_duration <- as.numeric(contr_t$sounding_duration)
contr_t <- contr_t %>%
  group_by(Rec_Session_Id) %>%
  mutate(mean_SRcol = mean(sounding_duration, na.rm = TRUE))

contr_t$sounding_duration[is.na(contr_t$sounding_duration)] <- contr_t$mean_SRcol[is.na(contr_t$sounding_duration)]

contr_t <- contr_t%>% rename(Speech_duration= sounding_duration)

#Labvanced has horrible names for their variables... let's change those
inter_t <- rename(inter_t, prodstim_pre = copy_of_copy_of_factor1_tg1_task5)
inter_t <- rename(inter_t, prodstim_post = copy_of_copy_of_copy_of_factor1_tg1_task5)
inter_t <- unite(inter_t, "prod_stim", prodstim_pre:prodstim_post, sep = "", remove = TRUE, na.rm = TRUE)
inter_t$Speech_duration <- as.character(inter_t$Speech_duration)
inter_t$Speech_duration <- as.numeric(inter_t$Speech_duration)
inter_t <- inter_t %>%
  group_by(Rec_Session_Id) %>%
  mutate(mean_SRcol = mean(Speech_duration, na.rm = TRUE))

inter_t$Speech_duration[is.na(inter_t$Speech_duration)] <- inter_t$mean_SRcol[is.na(inter_t$Speech_duration)]



inter_d <- rename(inter_d, prodstim_pre = copy_of_copy_of_factor1_tg1_task5)
inter_d <- rename(inter_d, prodstim_post = copy_of_copy_of_copy_of_factor1_tg1_task5)
inter_d <- unite(inter_d, "prod_stim", prodstim_pre:prodstim_post, sep = "", remove = TRUE, na.rm = TRUE)
inter_d <-  rename(inter_d, vot = X3)
inter_d <-  rename(inter_d, label = X2)
inter_d$Speech_duration <- as.character(inter_d$Speech_duration)
inter_d$Speech_duration <- as.numeric(inter_d$Speech_duration)
inter_d <- inter_d %>%
  group_by(Rec_Session_Id) %>%
  mutate(mean_SRcol = mean(Speech_duration, na.rm = TRUE))

inter_d$Speech_duration[is.na(inter_d$Speech_duration)] <- inter_d$mean_SRcol[is.na(inter_d$Speech_duration)]


inter_d_t <- subset(inter_d, prod_stim == "level_1")
inter_d_t <- subset(inter_d_t, label == 'd')
inter_d_t_pre <- subset(inter_d_t, Task_Name == "Production_pre")
inter_d_t_pre$vot <- as.character(inter_d_t_pre$vot)
inter_d_t_pre$vot <- as.numeric(inter_d_t_pre$vot)
inter_d_t_post <- subset(inter_d_t, Task_Name == "Production_Post")
inter_d_t_post$vot <- as.character(inter_d_t_post$vot)
inter_d_t_post$vot <- as.numeric(inter_d_t_post$vot)
i_d_t_pre <- inter_d_t_pre %>%
  group_by(Rec_Session_Id) %>%
  summarise(mean_vot = mean(vot))
i_d_t_pre <- i_d_t_pre %>% mutate(Task_Name = "Production_pre_d")

i_d_t_post <- inter_d_t_post %>%
  group_by(Rec_Session_Id) %>%
  summarise(mean_vot = mean(vot))
i_d_t_post <- i_d_t_post %>%mutate(Task_Name = "Production_post_d")

i_d_t <- rbind(i_d_t_post,i_d_t_pre)
interd_t_model <- rbind(inter_d_t_post, inter_d_t_pre)
m <- lmer(vot ~ Task_Name + (1|Rec_Session_Id), data = interd_t_model)
summary(m)
anova(m)
report(m)
ggplot(data = i_d_t, aes(x=Task_Name, y=mean_vot))+
  geom_boxplot()

#now the whol damn thing for inter_d_d
inter_d_d <- subset(inter_d, prod_stim == "level_2")
inter_d_d <- subset(inter_d_d, label == 'd')
inter_d_d_pre <- subset(inter_d_d, Task_Name == "Production_pre")
inter_d_d_pre$vot <- as.character(inter_d_d_pre$vot)
inter_d_d_pre$vot <- as.numeric(inter_d_d_pre$vot)
inter_d_d_post <- subset(inter_d_d, Task_Name == "Production_Post")
inter_d_d_post$vot <- as.character(inter_d_d_post$vot)
inter_d_d_post$vot <- as.numeric(inter_d_d_post$vot)
i_d_d_pre <- inter_d_d_pre %>%
  group_by(Rec_Session_Id) %>%
  summarise(mean_vot = mean(vot))
i_d_d_pre <- i_d_d_pre %>% mutate(Task_Name = "Production_pre_t")

i_d_d_post <- inter_d_d_post %>%
  group_by(Rec_Session_Id) %>%
  summarise(mean_vot = mean(vot))
i_d_d_post <- i_d_d_post %>%mutate(Task_Name = "Production_post_t")

i_d_d <- rbind(i_d_d_post,i_d_d_pre)

ggplot(data = i_d_d, aes(x=Task_Name, y=mean_vot))+
  geom_boxplot()
levels <- c('Production_pre_d', 'Production_post_d', 'Production_pre_t', 'Production_post_t') 

i_d <- rbind(i_d_d,i_d_t)
ggplot(data = i_d, aes(x=Task_Name, y=name))+
  geom_violin()+
  geom_boxplot(width = 0.1)+
  geom_point()+
  scale_x_discrete(limits = levels)+
  ggtitle('Speaking & Listening - /d/-bias')+
  ylab('VOT in ms')+
  xlab('Task')

interd_d_model <- rbind(inter_d_d_post, inter_d_d_pre)
interd_d_model$Speech_duration <- as.character(interd_d_model$Speech_duration)
interd_d_model$Speech_duration <- as.numeric(interd_d_model$Speech_duration)
interd_d_model <- interd_d_model %>% mutate(vot_norm = vot/Speech_duration)

m <- lmer(vot_norm ~ Task_Name + (1|Rec_Session_Id), data = interd_d_model)
summary(m)
anova(m)
report(m)

ggplot(data = interd_d_model, aes(x = Task_Name, y = Speech_duration))+
  geom_violin()+
  geom_boxplot(width = 0.1)+
  geom_point()

#now for interaction t_d

inter_t_d <- subset(inter_t, prod_stim == "level_2")
inter_t_d <- subset(inter_t_d, label == 'd')
inter_t_d_pre <- subset(inter_t_d, Task_Name == "Production_pre")
inter_t_d_pre$vot <- as.character(inter_t_d_pre$vot)
inter_t_d_pre$vot <- as.numeric(inter_t_d_pre$vot)
inter_t_d_post <- subset(inter_t_d, Task_Name == "Production_Post")
inter_t_d_post$vot <- as.character(inter_t_d_post$vot)
inter_t_d_post$vot <- as.numeric(inter_t_d_post$vot)
i_t_d_pre <- inter_t_d_pre %>%
  group_by(Rec_Session_Id) %>%
  summarise(mean_vot = mean(vot))
i_t_d_pre <- i_t_d_pre %>% mutate(Task_Name = "Production_pre_t")

i_t_d_post <- inter_t_d_post %>%
  group_by(Rec_Session_Id) %>%
  summarise(mean_vot = mean(vot))
i_t_d_post <- i_t_d_post %>%mutate(Task_Name = "Production_post_t")

i_t_d <- rbind(i_t_d_post,i_t_d_pre)

ggplot(data = i_t_d, aes(x=Task_Name, y=mean_vot))+
  geom_boxplot()

intert_d_model <- rbind(inter_t_d_post, inter_t_d_pre)


m <- lmer(vot ~ Task_Name + (1|Rec_Session_Id), data = intert_d_model)
summary(m)
anova(m)
report(m)


#now for interaction t_t

inter_t_t <- subset(inter_t, prod_stim == "level_1")
inter_t_t <- subset(inter_t_t, label == 'd')
inter_t_t_pre <- subset(inter_t_t, Task_Name == "Production_pre")
inter_t_t_pre$vot <- as.character(inter_t_t_pre$vot)
inter_t_t_pre$vot <- as.numeric(inter_t_t_pre$vot)
inter_t_t_post <- subset(inter_t_t, Task_Name == "Production_Post")
inter_t_t_post$vot <- as.character(inter_t_t_post$vot)
inter_t_t_post$vot <- as.numeric(inter_t_t_post$vot)
i_t_t_pre <- inter_t_t_pre %>%
  group_by(Rec_Session_Id) %>%
  summarise(mean_vot = mean(vot))
i_t_t_pre <- i_t_t_pre %>% mutate(Task_Name = "Production_pre_d")

i_t_t_post <- inter_t_t_post %>%
  group_by(Rec_Session_Id) %>%
  summarise(mean_vot = mean(vot))
i_t_t_post <- i_t_t_post %>%mutate(Task_Name = "Production_post_d")

i_t_t <- rbind(i_t_t_post,i_t_t_pre)

ggplot(data = i_t_t, aes(x=Task_Name, y=mean_vot))+
  geom_boxplot()

levels <- c('Production_pre_d', 'Production_post_d', 'Production_pre_t', 'Production_post_t') 


i_t <- rbind(i_t_d,i_t_t)
ggplot(data = i_t, aes(x=Task_Name, y=name))+
  geom_violin()+
  geom_boxplot(width = 0.1)+
  geom_point()+
  scale_x_discrete(limits = levels)+
  ggtitle('Speaking & Listening /t/- bias') +
  ylab('VOT in ms')+
  xlab('Task')

intert_t_model <- rbind(inter_t_t_post, inter_t_t_pre)


intert_t_model$Rec_Session_Id <- as.factor(intert_t_model$Rec_Session_Id)


intert_model <- rbind(intert_t_model, intert_d_model)
m <- lm(vot ~ Task_Name + (1|Rec_Session_Id), data = intert_t_model)
summary(m)
anova(m)
report(m)

#now the control group

contr_d_t <- subset(contr_d, prod_stim == "level_1")
contr_d_t <- subset(contr_d_t, label == 'd')
contr_d_t_pre <- subset(contr_d_t, Task_Name == "Production_pre")
contr_d_t_pre$vot <- as.character(contr_d_t_pre$vot)
contr_d_t_pre$vot <- as.numeric(contr_d_t_pre$vot)
contr_d_t_post <- subset(contr_d_t, Task_Name == "Production_Post")
contr_d_t_post$vot <- as.character(contr_d_t_post$vot)
contr_d_t_post$vot <- as.numeric(contr_d_t_post$vot)
c_d_t_pre <- contr_d_t_pre %>%
  group_by(Rec_Session_Id) %>%
  summarise(mean_vot = mean(vot))
c_d_t_pre <- c_d_t_pre %>% mutate(Task_Name = "Production_pre_d")

c_d_t_post <- contr_d_t_post %>%
  group_by(Rec_Session_Id) %>%
  summarise(mean_vot = mean(vot))
c_d_t_post <- c_d_t_post %>%mutate(Task_Name = "Production_post_d")

c_d_t <- rbind(c_d_t_post,c_d_t_pre)

ggplot(data = c_d_t, aes(x=Task_Name, y=mean_vot))+
  geom_violin()+
  geom_boxplot(width = 0.1)+
  geom_point()+
  scale_x_discrete(limits = levels)+
  ggtitle('Listening - /d/ bias')+
  ylab('VOT in ms')+
  xlab('Task')
contrd_t_model <- rbind(contr_d_t_post, contr_d_t_pre)
m <- lmer(vot ~ Task_Name + (1|Rec_Session_Id), data = contrd_t_model)
summary(m)
anova(m)
report(m)


#and control_d _d

contr_d_d <- subset(contr_d, prod_stim == "level_2")
contr_d_d <- subset(contr_d_d, label == 'd')
contr_d_d_pre <- subset(contr_d_d, Task_Name == "Production_pre")
contr_d_d_pre$vot <- as.character(contr_d_d_pre$vot)
contr_d_d_pre$vot <- as.numeric(contr_d_d_pre$vot)
contr_d_d_post <- subset(contr_d_d, Task_Name == "Production_Post")
contr_d_d_post$vot <- as.character(contr_d_d_post$vot)
contr_d_d_post$vot <- as.numeric(contr_d_d_post$vot)
c_d_d_pre <- contr_d_d_pre %>%
  group_by(Rec_Session_Id) %>%
  summarise(mean_vot = mean(vot))
c_d_d_pre <- c_d_d_pre %>% mutate(Task_Name = "Production_pre_t")

c_d_d_post <- contr_d_d_post %>%
  group_by(Rec_Session_Id) %>%
  summarise(mean_vot = mean(vot))
c_d_d_post <- c_d_d_post %>%mutate(Task_Name = "Production_post_t")

c_d_d <- rbind(c_d_d_post,c_d_d_pre)

ggplot(data = c_d_d, aes(x=Task_Name, y=name))+
  geom_boxplot()

levels <- c('Production_pre_d', 'Production_post_d', 'Production_pre_t', 'Production_post_t') 


c_d <- rbind(c_d_d,c_d_t)
ggplot(data = c_d, aes(x=Task_Name, y=name))+
  geom_violin()+
  geom_boxplot(width = 0.1)+
  geom_point()+
  scale_x_discrete(limits = levels)+
  ggtitle('Listening - /d/ bias')+
  ylab('VOT in ms')+
  xlab('Task')

contrd_d_model <- rbind(contr_d_d_post, contr_d_d_pre)
m <- lmer(vot ~ Task_Name + (1|Rec_Session_Id), data = contrd_d_model)
summary(m)
anova(m)
report(m)

#and the final control group: 

contr_t_d <- subset(contr_t, prod_stim == "level_2")
contr_t_d <- subset(contr_t_d, label == 'd')
contr_t_d_pre <- subset(contr_t_d, Task_Name == "Production_pre")
contr_t_d_pre$vot <- as.character(contr_t_d_pre$vot)
contr_t_d_pre$vot <- as.numeric(contr_t_d_pre$vot)
contr_t_d_post <- subset(contr_t_d, Task_Name == "Production_Post")
contr_t_d_post$vot <- as.character(contr_t_d_post$vot)
contr_t_d_post$vot <- as.numeric(contr_t_d_post$vot)
c_t_d_pre <- contr_t_d_pre %>%
  group_by(Rec_Session_Id) %>%
  summarise(mean_vot = mean(vot))
c_t_d_pre <- c_t_d_pre %>% mutate(Task_Name = "Production_pre_t")

c_t_d_post <- contr_t_d_post %>%
  group_by(Rec_Session_Id) %>%
  summarise(mean_vot = mean(vot))
c_t_d_post <- c_t_d_post %>%mutate(Task_Name = "Production_post_t")

c_t_d <- rbind(c_t_d_post,c_t_d_pre)

ggplot(data = c_t_d, aes(x=Task_Name, y=name))+
  geom_boxplot()


contrt_d_model <- rbind(contr_t_d_post, contr_t_d_pre)

m <- lmer(vot ~ Task_Name + (1|Rec_Session_Id), data = contrt_d_model)
summary(m)
anova(m)
report(m)

#the other level:
contr_t_t <- subset(contr_t, prod_stim == "level_1")
contr_t_t <- subset(contr_t_t, label == 'd')
contr_t_t_pre <- subset(contr_t_t, Task_Name == "Production_pre")
contr_t_t_pre$vot <- as.character(contr_t_t_pre$vot)
contr_t_t_pre$vot <- as.numeric(contr_t_t_pre$vot)
contr_t_t_post <- subset(contr_t_t, Task_Name == "Production_Post")
contr_t_t_post$vot <- as.character(contr_t_t_post$vot)
contr_t_t_post$vot <- as.numeric(contr_t_t_post$vot)
c_t_t_pre <- contr_t_t_pre %>%
  group_by(Rec_Session_Id) %>%
  summarise(mean_vot = mean(vot))
c_t_t_pre <- c_t_t_pre %>% mutate(Task_Name = "Production_pre_d")

c_t_t_post <- contr_t_t_post %>%
  group_by(Rec_Session_Id) %>%
  summarise(mean_vot = mean(vot))
c_t_t_post <- c_t_t_post %>%mutate(Task_Name = "Production_post_d")

c_t_t <- rbind(c_t_t_post,c_t_t_pre)

ggplot(data = c_t_t, aes(x=Task_Name, y=mean_vot))+
  geom_boxplot()

levels <- c('Production_pre_d', 'Production_post_d', 'Production_pre_t', 'Production_post_t') 


c_t <- rbind(c_t_d,c_t_t)
ggplot(data =c_t, aes(x=Task_Name, y=mean_vot))+
  geom_violin()+
  geom_boxplot(width = 0.1)+
  geom_point()+
  scale_x_discrete(limits = levels)+
  ggtitle('Listening /t/-bias')+
  ylab('VOT in ms')+
  xlab('Task')

contrt_t_model <- rbind(contr_t_t_post, contr_t_t_pre)

contrt_model <- rbind(contrt_t_model, contrt_d_model)

m <- lmer(vot ~ Task_Name + (1|Rec_Session_Id), data = contrt_t_model)
summary(m)
anova(m)
report(m)

#put this all into one nice file & plot 
#run stats

#now for perception: 
#filter out participants who did not appear in the production files, because their data was not useable
trials_contr_d <-  trials_and_sessions_cd %>% filter(!(Rec_Session_Id == 587551 |Rec_Session_Id == 587701 |Rec_Session_Id == 587716 |Rec_Session_Id == 600504 |Rec_Session_Id == 600505 |Rec_Session_Id == 600470 |Rec_Session_Id == 587745|Rec_Session_Id == 587515 | Rec_Session_Id == 618328))
contr_d_perc <- unite(trials_contr_d, "Vot_level", interaction, cftg1_, sep = "", remove = TRUE, na.rm = TRUE)
contr_d_perc <- unite(contr_d_perc, "Vot_level", Perc_Post_VOT,Vot_level, sep = "", remove = TRUE, na.rm = TRUE)


contr_d_perc_pre <-  subset(contr_d_perc, Task_Name == "Perception_Pre")
contr_d_perc_pre <- subset(contr_d_perc_pre, resp_pre == "tin")
contr_d_perc_pre$Vot_level <-  as.factor(contr_d_perc_pre$Vot_level)
contr_d_perc_pre$resp_pre <-  as.factor(contr_d_perc_pre$resp_pre)

count_per_pre_cd <- contr_d_perc_pre %>% group_by(Vot_level,resp_pre, .drop = TRUE) %>% tally()
perc_pre_cd <- contr_d_perc_pre %>% group_by(Rec_Session_Id, Vot_level, copy_of_of_copy_of_resp_prec, .drop = TRUE) %>% tally()
perc_pre_cd <- perc_pre_cd%>% rename(resp= copy_of_of_copy_of_resp_prec)
count_per_pre_cd <- count_per_pre_cd %>% mutate(task = "pre")
count_per_pre_cd <- count_per_pre_cd%>% mutate(prop = n/115)

contr_d_perc_int <-  subset(contr_d_perc, Task_Name == "interaction_d")
#inter_per_inter_it1 <- inter_t_perc_int %>% group_by(Vot_level, resp_int, .drop = FALSE) %>% tally()
contr_d_perc_int <- subset(contr_d_perc_int, resp_int == "tin")
contr_d_perc_int$Vot_level<-  as.factor(contr_d_perc_int$Vot_level)
contr_d_perc_int$resp_int <-  as.factor(contr_d_perc_int$resp_int)

count_per_int_cd <- contr_d_perc_int %>% group_by(Vot_level,resp_int, .drop = TRUE) %>% tally()
perc_int_cd <- contr_d_perc_int %>% group_by(Rec_Session_Id, Vot_level, copy_of_of_copy_of_resp_prec, .drop = TRUE) %>% tally()
perc_int_cd <- perc_int_cd%>% rename(resp= copy_of_of_copy_of_resp_prec)
count_per_int_cd <- count_per_int_cd %>% mutate(task = "inter")
count_per_int_cd <- count_per_int_cd%>% mutate(prop = n/230)

contr_d_perc_post <-  subset(contr_d_perc, Task_Name == "Perception_Post")
count_per_post_cd1 <- contr_d_perc_post %>% group_by(Vot_level, copy_of_of_copy_of_resp_prec, .drop = FALSE) %>% tally()
contr_d_perc_post <- subset(contr_d_perc_post, copy_of_of_copy_of_resp_prec == "tin")
contr_d_perc_post$Vot_level <-  as.factor(contr_d_perc_post$Vot_level)
contr_d_perc_post$copy_of_of_copy_of_resp_prec <-  as.factor(contr_d_perc_post$copy_of_of_copy_of_resp_prec)

count_per_post_cd <- contr_d_perc_post %>% group_by(Vot_level, copy_of_of_copy_of_resp_prec, .drop = TRUE) %>% tally()
perc_post_cd <- contr_d_perc_post %>% group_by(Rec_Session_Id, Vot_level, copy_of_of_copy_of_resp_prec, .drop = TRUE) %>% tally()
perc_post_cd <- perc_post_cd%>% rename(resp= copy_of_of_copy_of_resp_prec)
count_per_post_cd <- count_per_post_cd %>% mutate(task = "post")
count_per_post_cd <- count_per_post_cd%>% mutate(prop = n/115)

perc_cd <- rbind (count_per_post_cd, count_per_pre_cd, count_per_int_cd)
perc_cd$task <- as.factor(perc_cd$task)

contr_d_perc <- unite(contr_d_perc, "resp",resp_pre,copy_of_of_copy_of_resp_prec, resp_int, sep = "", remove = TRUE, na.rm = TRUE)
contr_d_model1<-  subset(contr_d_perc, Task_Name == "Perception_Pre")
contr_d_model1<-  contr_d_model1 %>% mutate(task = "pre")
contr_d_model2<-  subset(contr_d_perc, Task_Name == "interaction_d")
contr_d_model2<-  contr_d_model2 %>% mutate(task = "interaction")
contr_d_model3<-  subset(contr_d_perc, Task_Name ==  "Perception_Post")
contr_d_model3<-  contr_d_model3 %>% mutate(task = "post")
contr_d_model <- rbind(contr_d_model1, contr_d_model2, contr_d_model3)
contr_d_model$resp <-  as.factor(contr_d_model$resp)
contr_d_model$Task_Name <- as.factor(contr_d_model$Task_Name)
unique(contr_d_model[,'Rec_Session_Id'])

contr_d_model$Task_Name <- relevel(contr_d_model$Task_Name, "Perception_Pre")
m <-  glmer(resp ~ Task_Name + (1|Rec_Session_Id) + (1|Vot_level), family = binomial, data = contr_d_model)#
summary(m)
report(m)#perc_cd$n <- as.factor(perc_cd$n)

contr_d_mid3 <- subset(contr_d_model, Vot_level == "VOT_4"| Vot_level == 'VOT_5'| Vot_level == 'VOT_6')
m <-  glmer(resp ~  Task_Name + (1|Rec_Session_Id) , family = binomial, data = contr_d_mid3)#
summary(m) 
report(m)

ggplot(data = perc_cd, aes(x = Vot_level, y = prop, color = as.factor(task)))+
  geom_point(aes(group = task))+
  scale_color_discrete(name = "task")+
  geom_line(aes(group = task))+
  scale_color_discrete(name = "task")+
  xlab("VOT level")+
  scale_x_discrete(limits = level_order) +
  ylab('proportion of identification as t')+
  ggtitle("Listening - /d/ bias ")

#control condition t
trials_contr_t <-  trials_and_sessions_c_t %>% filter(!(Rec_Session_Id == 590768|Rec_Session_Id == 583084 |Rec_Session_Id == 584002 |Rec_Session_Id == 590638 | Rec_Session_Id ==590679| Rec_Session_Id == 590712 |Rec_Session_Id == 590766  |Rec_Session_Id == 591763| Rec_Session_Id == 595061 |Rec_Session_Id == 593626 |Rec_Session_Id == 593606 |Rec_Session_Id == 591952 |Rec_Session_Id == 593592 ))
contr_t_perc <- unite(trials_contr_t, "Vot_level", factor_interaction,cftg1, sep = "", remove = TRUE, na.rm = TRUE)

#contr_t_perc <- unite(trials_and_sessions_c_t, "Vot_level", factor_interaction,cftg1, sep = "", remove = TRUE, na.rm = TRUE)

contr_t_perc <- unite(contr_t_perc, "Vot_level", Perc_Post_VOT,Vot_level, sep = "", remove = TRUE, na.rm = TRUE)



contr_t_perc_pre <-  subset(contr_t_perc, Task_Name == "Perception_Pre")
contr_t_perc_pre <- subset(contr_t_perc_pre, copy_of_resp_prec == "tin")
contr_t_perc_pre$Vot_level <-  as.factor(contr_t_perc_pre$Vot_level)
contr_t_perc_pre$copy_of_resp_prec <-  as.factor(contr_t_perc_pre$copy_of_resp_prec)

count_per_pre_ct <- contr_t_perc_pre %>% group_by(Vot_level,copy_of_resp_prec, .drop = TRUE) %>% tally()
perc_pre_ct <- contr_t_perc_pre %>% group_by(Rec_Session_Id, Vot_level, of_copy_of_resp_prec, .drop = TRUE) %>% tally()
perc_pre_ct <- perc_pre_ct%>% rename(resp= of_copy_of_resp_prec)
count_per_pre_ct <- count_per_pre_ct %>% mutate(task = "pre")
count_per_pre_ct <- count_per_pre_ct %>% mutate(prop = n/110)


contr_t_perc_int <-  subset(contr_t_perc, Task_Name == "interaction_t")
inter_per_inter_it1 <- contr_t_perc_int %>% group_by(Vot_level, resp_prec_1, .drop = FALSE) %>% tally()
contr_t_perc_int <- subset(contr_t_perc_int, resp_prec_1 == "tin")
contr_t_perc_int$Vot_level<-  as.factor(contr_t_perc_int$Vot_level)
contr_t_perc_int$resp_prec_1 <-  as.factor(contr_t_perc_int$resp_prec_1)

count_per_int_ct <- contr_t_perc_int %>% group_by(Vot_level,resp_prec_1, .drop = TRUE) %>% tally()
perc_int_ct <- contr_t_perc_int %>% group_by(Rec_Session_Id, Vot_level, of_copy_of_resp_prec, .drop = TRUE) %>% tally()
perc_int_ct <- perc_int_ct%>% rename(resp= of_copy_of_resp_prec)
count_per_int_ct <- count_per_int_ct %>% mutate(task = "inter")
count_per_int_ct <- count_per_int_ct%>% mutate(prop = n/220)

contr_t_perc_post <-  subset(contr_t_perc, Task_Name == "Perception_Post")
count_per_post_ct1 <- contr_t_perc_post %>% group_by(Vot_level,of_copy_of_resp_prec, .drop = TRUE) %>% tally()
contr_t_perc_post <- subset(contr_t_perc_post, of_copy_of_resp_prec == "tin")
contr_t_perc_post$Vot_level <- as.factor(contr_t_perc_post$Vot_level)
contr_t_perc_post$of_copy_of_resp_prec <-  as.factor(contr_t_perc_post$of_copy_of_resp_prec)

count_per_post_ct <- contr_t_perc_post %>% group_by(Vot_level,of_copy_of_resp_prec, .drop = TRUE) %>% tally()
perc_post_ct <- contr_t_perc_post %>% group_by(Rec_Session_Id, Vot_level, of_copy_of_resp_prec, .drop = TRUE) %>% tally()
perc_post_ct <- perc_post_ct%>% rename(resp= of_copy_of_resp_prec)
count_per_post_ct <- count_per_post_ct %>% mutate(task = "post")
count_per_post_ct <- count_per_post_ct %>% mutate(prop = n/110)

perc_ct <- rbind (count_per_post_ct, count_per_pre_ct, count_per_int_ct)
perc_ct$task <- as.factor(perc_ct$task)

contr_t_perc <- unite(contr_t_perc, "resp",resp_prec_1,of_copy_of_resp_prec, copy_of_resp_prec, sep = "", remove = TRUE, na.rm = TRUE)
contr_t_model1<-  subset(contr_t_perc, Task_Name == "Perception_Pre")
contr_t_model1<-  contr_t_model1 %>% mutate(task = "pre")
contr_t_model2<-  subset(contr_t_perc, Task_Name == "interaction_t")
contr_t_model2<-  contr_t_model2 %>% mutate(task = "interaction")
contr_t_model3<-  subset(contr_t_perc, Task_Name ==  "Perception_Post")
contr_t_model3<-  contr_t_model3 %>% mutate(task = "post")
contr_t_model <- rbind(contr_t_model1, contr_t_model2, contr_t_model3)
contr_t_model$resp <-  as.factor(contr_t_model$resp)

contr_t_model$Vot_level <-  as.factor(contr_t_model$Vot_level)
contr_t_model$Task_Name <- as.factor(contr_t_model$Task_Name)
#unique(contr_t_model[,'Rec_Session_Id'])

contr_t_model$Task_Name <- relevel(contr_t_model$Task_Name, "Perception_Post")

m <-  glmer(resp ~ Task_Name + (1|Rec_Session_Id), family = binomial, data = contr_t_model)#
summary(m)
report(m)

contr_t_mid3 <- subset(contr_t_model, Vot_level == "VOT_4"| Vot_level == 'VOT_5'| Vot_level == 'VOT_6')
m <-  glmer(resp ~  Task_Name + (1|Rec_Session_Id) , family = binomial, data = contr_t_mid3)#
summary(m) 
report(m)

#perc_cd$n <- as.factor(perc_cd$n)
level_order <- c('VOT_1', 'VOT_2', 'VOT_3', 'VOT_4', 'VOT_5', 'VOT_6', 'VOT_7', 'VOT_8', 'VOT_9') 

ggplot(data = perc_ct, aes(x = Vot_level, y = prop, color = as.factor(task)))+
  geom_point(aes(group = task))+
  scale_color_discrete(name = "task")+
  geom_line(aes(group = task))+
  scale_color_discrete(name = "task")+
  xlab("VOT level")+
  scale_x_discrete(limits = level_order) +
  ylab('proportion of identification as t')+
  ggtitle("Listening- /t/ bias ")
  


#and last one
trials_intert <-  trials_and_sessions_t %>% filter(!(Rec_Session_Id == 581891 |Rec_Session_Id == 598831 |Rec_Session_Id == 597681|Rec_Session_Id == 584135 |Rec_Session_Id == 581723 | Rec_Session_Id == 595322 |  Rec_Session_Id == 598289))

inter_t_perc <- unite(trials_intert, "Vot_level", factor_interaction,cftg1, sep = "", remove = TRUE, na.rm = TRUE)
inter_t_perc <- unite(inter_t_perc, "Vot_level", Perc_Post_VOT,Vot_level, sep = "", remove = TRUE, na.rm = TRUE)


inter_t_perc_pre <-  subset(inter_t_perc, Task_Name == "Perception_Pre")
inter_per_pre_it1 <- inter_t_perc_pre %>% group_by(Vot_level, copy_of_resp_prec, .drop = FALSE) %>% tally()
inter_t_perc_pre <- subset(inter_t_perc_pre, copy_of_resp_prec == "tin")
inter_t_perc_pre$Vot_level <-  as.factor(inter_t_perc_pre$Vot_level)
inter_t_perc_pre$copy_of_resp_prec <-  as.factor(inter_t_perc_pre$copy_of_resp_prec)

count_per_pre_it <- inter_t_perc_pre %>% group_by(Vot_level,copy_of_resp_prec, .drop = TRUE) %>% tally()

perc_pre_it <- inter_t_perc_int %>% group_by(Rec_Session_Id, Vot_level, resp_prec_1, .drop = TRUE) %>% tally()
perc_pre_it <- perc_pre_it%>% rename(resp= resp_prec_1)
count_per_pre_it <- count_per_pre_it %>% mutate(task = "pre")
count_per_pre_it <- count_per_pre_it%>% mutate(prop = n/110)

inter_t_perc_post <-  subset(inter_t_perc, Task_Name == "Perception_Post")
inter_t_perc_post <- subset(inter_t_perc_post, of_copy_of_resp_prec == "tin")
inter_t_perc_post$Vot_level<-  as.factor(inter_t_perc_post$Vot_level)
inter_t_perc_post$of_copy_of_resp_prec <-  as.factor(inter_t_perc_post$of_copy_of_resp_prec)

count_per_post_it <- inter_t_perc_post %>% group_by(Vot_level,of_copy_of_resp_prec, .drop = TRUE) %>% tally()
perc_post_it <- inter_t_perc_post %>% group_by(Rec_Session_Id, Vot_level, resp_prec_1, .drop = TRUE) %>% tally()
perc_post_it <- perc_post_it%>% rename(resp= resp_prec_1)
count_per_post_it <- count_per_post_it %>% mutate(task = "post")
count_per_post_it <- count_per_post_it%>% mutate(prop = n/110)

inter_t_perc_int <-  subset(inter_t_perc, Task_Name == "interaction_t")
inter_per_inter_it1 <- inter_t_perc_int %>% group_by(Vot_level, resp_prec_1, .drop = FALSE) %>% tally()
inter_t_perc_int <- subset(inter_t_perc_int, resp_prec_1 == "tin")
inter_t_perc_int$Vot_level<-  as.factor(inter_t_perc_int$Vot_level)
inter_t_perc_int$resp_prec_1 <-  as.factor(inter_t_perc_int$resp_prec_1)

count_per_int_it <- inter_t_perc_int %>% group_by(Vot_level,resp_prec_1, .drop = TRUE) %>% tally()
perc_int_it <- inter_t_perc_int %>% group_by(Rec_Session_Id, Vot_level, resp_prec_1, .drop = TRUE) %>% tally()
perc_int_it <- perc_int_it%>% rename(resp= resp_prec_1)
count_per_int_it <- count_per_int_it %>% mutate(task = "inter")
count_per_int_it <- count_per_int_it%>% mutate(prop = n/220)

perc_it <- rbind (count_per_post_it, count_per_pre_it, count_per_int_it)
perc_it$task <- as.factor(perc_it$task)

inter_t_perc <- unite(inter_t_perc, "resp",resp_prec_1,of_copy_of_resp_prec, copy_of_resp_prec, sep = "", remove = TRUE, na.rm = TRUE)
inter_t_model1<-  subset(inter_t_perc, Task_Name == "Perception_Pre")
inter_t_model1<-  inter_t_model1 %>% mutate(task = "pre")
inter_t_model2<-  subset(inter_t_perc, Task_Name == "interaction_t")
inter_t_model2<-  inter_t_model2 %>% mutate(task = "interaction")
inter_t_model3<-  subset(inter_t_perc, Task_Name ==  "Perception_Post")
inter_t_model3<-  inter_t_model3 %>% mutate(task = "post")
inter_t_model <- rbind(inter_t_model1, inter_t_model2, inter_t_model3)
inter_t_model$resp <-  as.factor(inter_t_model$resp)
inter_t_model$Task_Name <- as.factor(inter_t_model$Task_Name)
unique(inter_t_model[,"Rec_Session_Id"])




inter_t_model$Task_Name <- relevel(inter_t_model$Task_Name, "Perception_Pre")

m <-  glmer(resp ~  Task_Name* (1|Rec_Session_Id)+ (1|Vot_level), family = binomial, data = inter_t_model)#
summary(m) 
report(m)

m <-  glmer(resp ~  Task_Name * (1|Rec_Session_Id), family = binomial, data = inter_t_model)#
summary(m) 
report(m)

inter_t_mid3 <- subset(inter_t_model, Vot_level == "VOT_4"| Vot_level == 'VOT_5'| Vot_level == 'VOT_6')
m <-  glmer(resp ~  Task_Name + (1|Rec_Session_Id)  , family = binomial, data = inter_t_mid3)#
summary(m) 
report(m)
#perc_cd$n <- as.factor(perc_cd$n)

level_order <- c('VOT_1', 'VOT_2', 'VOT_3', 'VOT_4', 'VOT_5', 'VOT_6', 'VOT_7', 'VOT_8', 'VOT_9') 

ggplot(data = perc_it, aes(x = Vot_level, y = prop, color = as.factor(task)))+
  geom_point(aes(group = task))+
  scale_color_discrete(name = "task")+
  geom_line(aes(group = task))+
  scale_x_discrete(limits=level_order)+
  scale_color_discrete(name = "task")+
  xlab("VOT level")+
  ylab('proportion of identification as d')+
  ggtitle("Listening & speaking - t bias")

#last one

trials_inter_d <-  trials_and_sessions_d %>% filter(!(Rec_Session_Id == 582438 | Rec_Session_Id == 587508 |Rec_Session_Id == 616404 |Rec_Session_Id == 582022 |Rec_Session_Id == 582024 |Rec_Session_Id == 582042 | Rec_Session_Id == 584540| Rec_Session_Id == 584563 |Rec_Session_Id == 584565 | Rec_Session_Id == 584571|  Rec_Session_Id == 584579|  Rec_Session_Id == 584587|  Rec_Session_Id == 599116|  Rec_Session_Id == 599117 |  Rec_Session_Id == 599933))

inter_d_perc <- unite(trials_inter_d, "Vot_level", interaction, cftg1_, sep = "", remove = TRUE, na.rm = TRUE)
inter_d_perc <- unite(inter_d_perc, "Vot_level", Perc_Post_VOT,Vot_level, sep = "", remove = TRUE, na.rm = TRUE)


inter_d_perc_pre <-  subset(inter_d_perc, Task_Name == "Perception_Pre")
count_per_pre_id1 <- inter_d_perc_pre %>% group_by(Vot_level, resp_pre, .drop = FALSE) %>% tally()
inter_d_perc_pre <- subset(inter_d_perc_pre, resp_pre == "tin")
inter_d_perc_pre$Vot_level <-  as.factor(inter_d_perc_pre$Vot_level)
inter_d_perc_pre$resp_pre <-  as.factor(inter_d_perc_pre$resp_pre)

count_per_pre_id <- inter_d_perc_pre %>% group_by(Vot_level,resp_pre, .drop = TRUE) %>% tally()
perc_pre_id <- inter_d_perc_pre %>% group_by(Rec_Session_Id, Vot_level, copy_of_of_copy_of_resp_prec, .drop = TRUE) %>% tally()
perc_pre_id <- perc_pre_id%>% rename(resp= copy_of_of_copy_of_resp_prec)
count_per_pre_id <- count_per_pre_id %>% mutate(task = "pre")
count_per_pre_id <- count_per_pre_id%>% mutate(prop = n/110)

inter_d_perc_int <-  subset(inter_d_perc, Task_Name == "interaction_d")
inter_per_inter_it1 <- inter_d_perc_int %>% group_by(Vot_level, resp_int , .drop = FALSE) %>% tally()
inter_d_perc_int <- subset(inter_d_perc_int, resp_int == "tin")
inter_d_perc_int$Vot_level<-  as.factor(inter_d_perc_int$Vot_level)
inter_d_perc_int$resp_int <-  as.factor(inter_d_perc_int$resp_int)

count_per_int_id <- inter_d_perc_int %>% group_by(Vot_level,resp_int, .drop = TRUE) %>% tally()
perc_int_id <- inter_d_perc_int %>% group_by(Rec_Session_Id, Vot_level, copy_of_of_copy_of_resp_prec, .drop = TRUE) %>% tally()
perc_int_id <- perc_int_id%>% rename(resp= copy_of_of_copy_of_resp_prec)
count_per_int_id <- count_per_int_id %>% mutate(task = "inter")
count_per_int_id <- count_per_int_id%>% mutate(prop = n/220)

inter_d_perc_post <-  subset(inter_d_perc, Task_Name == "Perception_Post")
inter_d_perc_post <- subset(inter_d_perc_post, copy_of_of_copy_of_resp_prec == "tin")
inter_d_perc_post$Vot_level <-  as.factor(inter_d_perc_post$Vot_level)
inter_d_perc_post$copy_of_of_copy_of_resp_prec <-  as.factor(inter_d_perc_post$copy_of_of_copy_of_resp_prec)

count_per_post_id <- inter_d_perc_post %>% group_by(Vot_level, copy_of_of_copy_of_resp_prec, .drop = TRUE) %>% tally()
perc_post_id <- inter_d_perc_post %>% group_by(Rec_Session_Id, Vot_level, copy_of_of_copy_of_resp_prec, .drop = TRUE) %>% tally()
perc_post_id <- perc_post_id%>% rename(resp= copy_of_of_copy_of_resp_prec)
count_per_post_id <- count_per_post_id %>% mutate(task = "post")
count_per_post_id <- count_per_post_id%>% mutate(prop = n/110)

perc_id <- rbind (count_per_post_id, count_per_pre_id, count_per_int_id)
perc_id$task <- as.factor(perc_id$task)
#perc_id <- perc_id %>% mutate(prop = n/160)
#perc_cd$n <- as.factor(perc_cd$n)

perc_id <- perc_id %>% mutate(group = "interactive")
perc_id <- perc_id %>% mutate(bias = "/d/")
perc_id <- perc_id %>% mutate(cond = "interd")
perc_it <- perc_it %>% mutate(group = "interactive")
perc_it <- perc_it %>% mutate(bias = "/t/")
perc_it <- perc_it %>% mutate(cond = "intert")
perc_cd <- perc_cd %>% mutate(group = "control")
perc_cd <- perc_cd %>% mutate(bias = "/d/")
perc_cd <- perc_cd %>% mutate(cond = "contrd")
perc_ct <- perc_ct %>% mutate(group = "control")
perc_ct <- perc_ct %>% mutate(bias = "/t/")
perc_ct <- perc_ct %>% mutate(cond = "contrt")

perc_all <- rbind(perc_it, perc_id, perc_ct, perc_cd)

level_order <- c('VOT_1', 'VOT_2', 'VOT_3', 'VOT_4', 'VOT_5', 'VOT_6', 'VOT_7', 'VOT_8', 'VOT_9') 

ggplot(data = perc_pre_all, aes(x = Vot_level , y = prop, color = cond))+
  geom_point(aes(group = cond))+
  scale_x_discrete(limits = level_order, labels = c('1','2','3', '4', '5','6','7','8','9'))+
  geom_line(aes(group = cond))+
  scale_fill_discrete(name = "task", labels=c("pre","post","interactive"))+
  xlab(label  = "Stimulus level from /din/ to /tin/")+
  ylab(label = "proportion of identification as /tin/")
  

perc_pre_all <- subset(perc_all, task == "pre")

level_order <- c('VOT_1', 'VOT_2', 'VOT_3', 'VOT_4', 'VOT_5', 'VOT_6', 'VOT_7', 'VOT_8', 'VOT_9') 


perc_all <- perc_all %>% mutate(treat = case_when(group == "interactive" ~ "speaking & listening", group == "control"~ "listening"))

ggplot(data = perc_all, aes(x = Vot_level , y = prop, color = task))+
  geom_point(aes(group = task))+
  scale_x_discrete(limits = level_order, labels = c('1','2','3', '4', '5','6','7','8','9'))+
  geom_line(aes(group = task))+
  scale_fill_discrete(name = "task", labels=c("pre","post","interactive"))+
  theme_bw()+
  xlab(label  = "Stimulus level from /din/ to /tin/")+
  ylab(label = "proportion of identification as /tin/")+
  scale_color_manual(values = wes_palette("Moonrise3"))+
  facet_grid(treat ~ bias)
  geom_text(aes(size = 10))

perc_pre_it <- perc_pre_it %>% mutate(Condition = "inter")
perc_pre_ct <- perc_pre_ct %>% mutate(Condition = "contr")
perc_post_it <- perc_post_it %>% mutate(Condition = "inter")
perc_post_ct <- perc_post_ct %>% mutate(Condition = "contr")

perc_t_pre_count <- rbind(perc_pre_it, perc_pre_ct)
perc_t_post_count <- rbind(perc_post_it, perc_post_ct)
perc_t_ppcount <- merge(perc_t_post_count, perc_t_pre_count, by = "Rec_Session_Id")

m <- aov(n.x ~ Condition.x + n.y, data = perc_t_ppcount)
summary(m)
Anova(m,type= 'III')

perc_pre_id <- perc_pre_id %>% mutate(Condition = "inter")
perc_pre_cd <- perc_pre_cd %>% mutate(Condition = "contr")
perc_post_id <- perc_post_id %>% mutate(Condition = "inter")
perc_post_cd <- perc_post_cd %>% mutate(Condition = "contr")

perc_d_pre_count <- rbind(perc_pre_id, perc_pre_cd)
perc_d_post_count <- rbind(perc_post_id, perc_post_cd)
perc_d_ppcount <- merge(perc_d_post_count, perc_d_pre_count, by = "Rec_Session_Id")

m <- aov(n.x ~ Condition.x + n.y, data = perc_d_ppcount)
summary(m)
Anova(m,type= 'III')
  

inter_d_perc <- unite(inter_d_perc, "resp",resp_pre,copy_of_of_copy_of_resp_prec, resp_int, sep = "", remove = TRUE, na.rm = TRUE)
inter_d_model1<-  subset(inter_d_perc, Task_Name == "Perception_Pre")
inter_d_model1<-  inter_d_model1 %>% mutate(task = "pre")
inter_d_model2<-  subset(inter_d_perc, Task_Name == "interaction_d")
inter_d_model2<-  inter_d_model2 %>% mutate(task = "interaction")
inter_d_model3<-  subset(inter_d_perc, Task_Name ==  "Perception_Post")
inter_d_model3<-  inter_d_model3 %>% mutate(task = "post")

inter_d_model <- rbind(inter_d_model1, inter_d_model2, inter_d_model3)
inter_d_model$resp <-  as.factor(inter_d_model$resp)
inter_d_model$Task_Name <- as.factor(inter_d_model$Task_Name)
inter_d_model$Vot_level <- as.factor(inter_d_model$Vot_level)
#unique(inter_d_model[,'Rec_Session_Id'])


inter_d_perc <-  inter_d_model %>% select(Rec_Session_Id, Task_Name, resp,  Vot_level, task)
inter_d_perc  <- inter_d_perc%>% mutate(condition = 'inter_d', treatment = 'interaction', bias ='d')

inter_t_perc <-  inter_t_model %>% select(Rec_Session_Id, Task_Name, resp,  Vot_level, task)
inter_t_perc  <- inter_t_perc%>% mutate(condition = 'inter_t', treatment = 'interaction', bias ='t')

contr_d_perc <-  contr_d_model %>% select(Rec_Session_Id, Task_Name, resp, Vot_level, task)
contr_d_perc  <- contr_d_perc%>% mutate(condition = 'contr_d', treatment = 'control', bias ='d')

contr_t_perc <-  contr_t_model %>% select(Rec_Session_Id, Task_Name, resp, Vot_level, task)
contr_t_perc  <- contr_t_perc%>% mutate(condition = 'contr_t', treatment = 'control', bias ='t')

perc_all_model <- rbind(inter_t_perc,inter_d_perc,contr_d_perc, contr_t_perc)
perc_all_model$Vot_level <- as.factor(perc_all_model$Vot_level)

perc_t_model <-  rbind(inter_t_perc, contr_t_perc)
perc_d_model <-  rbind(inter_d_perc, contr_d_perc)
perc_t_model_int <-  subset(perc_t_model, Task_Name =='interaction_t')
perc_t_model_post <-  subset(perc_t_model, Task_Name =='Perception_Post')
perc_t_model_pre <-  subset(perc_t_model, Task_Name =='Perception_Pre')
perc_d_model_int <-  subset(perc_d_model, Task_Name =='interaction_d')
perc_d_model_post <-  subset(perc_d_model, Task_Name =='Perception_Post')
perc_d_model_pre <-  subset(perc_d_model, Task_Name =='Perception_Pre')

perc_t_prepost <- merge(perc_t_model_post, perc_t_model_pre, by = 'Rec_Session_Id')
perc_t_prepost$resp.x <-  as.factor(perc_t_prepost$resp.x)
perc_t_prepost$resp.y <-  as.factor(perc_t_prepost$resp.y)

m1 <- aov(resp.x ~ condition.x* Vot_level.x + resp.y, data= perc_t_prepost)
Anova(m1, type = 'III')


m <- glmer(resp ~ condition + (1|Rec_Session_Id)  , family = binomial, data = perc_d_model_int)#
summary(m)
report(m)



inter_d_model$Task_Name <- relevel(inter_d_model$Task_Name, "Perception_Post")
perc_all_model$task <- as.factor(perc_all_model$task)
perc_all_model$Rec_Session_Id <- as.factor(perc_all_model$Rec_Session_Id)
perc_all_model$task <- relevel(perc_all_model$task, "pre")
perc_all_model$Vot_level <- as.factor(perc_all_model$Vot_level)
perc_all_model$stim_level <- as.ordered(perc_all_model$stim_level)
perc_all_model <- perc_all_model %>% mutate(stim_level = case_when(Vot_level == "VOT_1"~ 1, 
                                                                   Vot_level == "VOT_2"~ 2,
                                                                   Vot_level == "VOT_3"~ 3,
                                                                   Vot_level == "VOT_4"~ 4,
                                                                   Vot_level == "VOT_5"~ 5,
                                                                   Vot_level == "VOT_6"~ 6, 
                                                                   Vot_level == "VOT_7"~ 7, 
                                                                   Vot_level == "VOT_8"~ 8, 
                                                                   Vot_level == "VOT_9"~ 9))
#perc_all_model$stim_level <- as.numeric(perc_all_model$stim_level)

perc_all_model$stim_level<- ordered(perc_all_model$stim_level, 
                              levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9))


#the model for perception


#create a df with only the mid 3 stimuli: 
perc_all_mid3 <- subset(perc_all_model, Vot_level == "VOT_4"| Vot_level == 'VOT_5'| Vot_level == 'VOT_6')

#try a model on the whole continuum first: 
#the model that does not work: 
m3 <-  glmer(resp ~ bias * task * stim_level + (1|Rec_Session_Id), family = binomial,  data = perc_all_model)
summary(m3)
anova(m3)
plot(m3)


#try something simpler
m0 <- glmer(resp ~ condition * task + (1|Rec_Session_Id), family = binomial (link = 'logit'), data = perc_all_model, glmerControl(optimizer = "bobyqa"))
summary(m0)


emm0 <- emmeans(m0,  ~ condition * task)
pairs(emm0, adjust = "fdr")


#now try with a random slope: 
#doesn't converge
m4 <- glmer(resp ~ condition * task * stim_level + (1 + stim_level|Rec_Session_Id), family = binomial (link = 'logit'), data = perc_all_model, glmerControl(optimizer = "bobyqa"))

#also singular
m5 <- glmer(resp ~ condition * task + (1 + Vot_level|Rec_Session_Id), family = binomial (link = 'logit'), data = perc_all_model, glmerControl(optimizer = "bobyqa"))
summary(m5)
report(m5)
lmerTest(m5)

#so let's try for only the middle 3 stimuli: 

 #It works
 perc_all_mid3$Vot_level <-  as.ordered (perc_all_mid3$Vot_level)
 m6 <- glmer(resp ~ condition * task + (1 + Vot_level|Rec_Session_Id), family = binomial (link = 'logit'), data = perc_all_mid3, glmerControl((optimizer = "bobyqa")))
 summary(m6)
 report(m6)

#pairwise: 
 emm6 <- emmeans(m6,  ~ condition * task)
 contrast(emm6, method = "pairwise")
 emm6pairs <-  pairs(emm6, adjust = "fdr")
 print(emm6pairs)
 
 #filter out the data I actually want
 target_strings_perc_with <-  c("contr_d pre - contr_d post",
 "contr_d pre - contr_d interaction",
 "contr_d interaction - contr_d post",

 
 "contr_t pre - contr_t post",
 "contr_t pre - contr_t interaction",
 "contr_t interaction - contr_t post",
 
 "inter_d pre - inter_d post",
 "inter_d pre - inter_d interaction",
 "inter_d interaction - inter_d post",
 
 
 "inter_t pre - inter_t post",
 "inter_t pre - inter_t interaction",
 "inter_t interaction - inter_t post")
 
 
 #the other comparison
target_strings_all <- c("contr_d pre - contr_t pre", 
"contr_d pre - inter_d pre",
"contr_d pre - inter_t pre", 
"contr_d pre - contr_d interaction",
"contr_d pre - contr_t interaction", 
"contr_d pre - inter_d interaction", 
"contr_d pre - inter_t interaction",
"contr_d pre - contr_d post",
"contr_d pre - contr_t post", 
"contr_d pre - inter_d post",
"contr_d pre - inter_t post", 
"contr_t pre - inter_d pre",
"contr_t pre - inter_t pre", 
"contr_t pre - contr_d interaction",
"contr_t pre - contr_t interaction", 
"contr_t pre - inter_d interaction",
"contr_t pre - inter_t interaction", 
"contr_t pre - contr_d post",
"contr_t pre - contr_t post",
"contr_t pre - inter_d post",
"contr_t pre - inter_t post",
"inter_d pre - inter_t pre", 
"inter_d pre - contr_d interaction",
"inter_d pre - contr_t interaction",
"inter_d pre - inter_d interaction", 
"inter_d pre - inter_t interaction",
"inter_d pre - contr_d post",
"inter_d pre - contr_t post",
"inter_d pre - inter_d post",
"inter_d pre - inter_t post",
"inter_t pre - contr_d interaction", 
"inter_t pre - contr_t interaction", 
"inter_t pre - inter_d interaction",
"inter_t pre - inter_t interaction", 
"inter_t pre - contr_d post",
"inter_t pre - contr_t post",
"inter_t pre - inter_d post", 
"inter_t pre - inter_t post",
"contr_d interaction - contr_t interaction",
"contr_d interaction - inter_d interaction",
"contr_d interaction - inter_t interaction",
"contr_d interaction - contr_d post", 
"contr_d interaction - contr_t post",
"contr_d interaction - inter_d post",
"contr_d interaction - inter_t post",
"contr_t interaction - inter_d interaction",
"contr_t interaction - inter_t interaction", 
"contr_t interaction - contr_d post", 
"contr_t interaction - contr_t post",
"contr_t interaction - inter_d post",
"contr_t interaction - inter_t post",
"inter_d interaction - inter_t interaction",
"inter_d interaction - contr_d post",
"inter_d interaction - contr_t post", 
"inter_d interaction - inter_d post", 
"inter_d interaction - inter_t post",
"inter_t interaction - contr_d post", 
"inter_t interaction - contr_t post", 
"inter_t interaction - inter_d post", 
"inter_t interaction - inter_t post",
"contr_d post - contr_t post",
"contr_d post - inter_d post",
"contr_d post - inter_t post", 
"contr_t post - inter_d post", 
"contr_t post - inter_t post", 
"inter_d post - inter_t post")
 
 as.data.frame(emm6pairs)
 
 emm6pairs <- as.data.table(emm6pairs)
 # Subset the rows based on the specified strings
 #pairs_filt <- em87pairs_filt[grepl(paste(target_strings, collapse = "|"), em87pairs_filt$contrast), ]
 emm6pairsfilt<-  emm6pairs[grepl(paste(target_stringsperc_with, collapse = "|"), contrast)]
 print(emm6pairsfilt)
 








 #now back to prodcution

#select the dfs you need/want
inter_d_bind <-  inter_d %>% select(Rec_Session_Id, Task_Name, prod_stim, vot, label, Speech_duration)
inter_d_bind  <- inter_d_bind %>% mutate(condition = 'inter_d', treatment = 'interaction', bias ='d')

inter_t_bind <-  inter_t %>% select(Rec_Session_Id, Task_Name, prod_stim, vot, label, Speech_duration)
inter_t_bind  <- inter_t_bind %>% mutate(condition = 'inter_t', treatment = 'interaction', bias ='t')

contr_d_bind <-  contr_d %>% select(Rec_Session_Id, Task_Name, prod_stim, vot, label, Speech_duration)
contr_d_bind  <- contr_d_bind %>% mutate(condition = 'contr_d', treatment = 'control', bias ='d')

contr_t_bind <-  contr_t %>% select(Rec_Session_Id, Task_Name, prod_stim, vot, label, Speech_duration)
contr_t_bind  <- contr_t_bind %>% mutate(condition = 'contr_t', treatment = 'control', bias ='t')



prod_all <- rbind(inter_d_bind,inter_t_bind,contr_d_bind,contr_t_bind)
prod_all$vot <- as.numeric(as.character(prod_all$vot))
prod_all$Rec_Session_Id <- as.factor(prod_all$Rec_Session_Id)
prod_all$treatment <- as.factor(prod_all$treatment)
prod_all$bias <- as.factor(prod_all$bias)
prod_all$Rec_Session_Id <- as.factor(prod_all$Rec_Session_Id)
prod_all$prod_stim <- as.factor(prod_all$prod_stim)
prod_all$Task_Name<- as.factor(prod_all$Task_Name)
prod_all <-  prod_all %>% subset(label == "d")
prod_all <-prod_all%>%
  mutate(Stimulus = case_when(prod_stim == "level_1" ~ "din",
        prod_stim == "level_2" ~ "tin"))
prod_all <- prod_all %>% mutate(vot_norm = vot/Speech_duration)

prod_pre <- subset(prod_all, Task_Name == "Production_pre")
prod_pre_t <- subset(prod_pre, prod_stim == "level_2")
prod_pre_t_t <- subset(prod_pre_t, bias =='t')
prod_pre_d_t <- subset(prod_pre_t, bias =='d')
prod_pre_d <- subset(prod_pre, prod_stim == "level_1")
prod_pre_t_d <- subset(prod_pre_d, bias =='t')
prod_pre_d_d <- subset(prod_pre_d, bias =='d')
prod_post <- subset(prod_all, Task_Name == "Production_Post")
prod_post_t <- subset(prod_post, prod_stim == "level_2")
prod_post_t_t <- subset(prod_post_t, bias =='t')
prod_post_d_t <- subset(prod_post_t, bias =='d')
prod_post_d <- subset(prod_post, prod_stim == "level_1")
prod_post_t_d <- subset(prod_post_d, bias =='t')
prod_post_d_d <- subset(prod_post_d, bias =='d')

prod_pre <- prod_pre %>% rename(vot_pre = vot)
prod_pre_t <- prod_pre_t %>% rename(vot_pre = vot)
prod_pre_d <- prod_pre_d %>% rename(vot_pre = vot)
prod_pre_t_t <- prod_pre_t_t%>% rename(vot_pre=vot)
prod_pre_d_t <- prod_pre_d_t%>% rename(vot_pre=vot)
prod_pre_t_d <- prod_pre_t_d%>% rename(vot_pre=vot)
prod_pre_d_d <- prod_pre_d_d%>% rename(vot_pre=vot)

Prod_pre_t_mean <-prod_pre_t %>% group_by(Rec_Session_Id) %>%
  summarise(mean_vot_pre = mean(vot_pre))
Prod_post_t_mean <-prod_post_t %>% group_by(Rec_Session_Id) %>%
  summarise(mean_vot_post = mean(vot))

prod_t_mean <- merge(x = Prod_pre_t_mean, y = Prod_post_t_mean, by = "Rec_Session_Id")

prod_all_t <-  merge( x = prod_pre_t, y = prod_post_t, by = "Rec_Session_Id", all.x = FALSE)
prod_all_t$vot_pre <-  as.factor(prod_all_t$vot_pre)
prod_all_d <-  merge( x = prod_post_d, y = prod_pre_d, by = "Rec_Session_Id", all.x = TRUE)
prod_ppall <-  merge( x = prod_post, y = prod_pre, by = "Rec_Session_Id", all.x = TRUE)
prod_t_t <- merge(prod_post_t_t, prod_pre_t_t, by = "Rec_Session_Id", all.x = TRUE)
prod_d_t <- merge(prod_post_d_t, prod_pre_d_t, by = "Rec_Session_Id", all.x = TRUE)
prod_d_d <- merge(prod_post_d_d, prod_pre_d_d, by = "Rec_Session_Id", all.x = TRUE)
prod_t_d <- merge(prod_post_t_d, prod_pre_t_d, by = "Rec_Session_Id", all.x = TRUE)
prod_t_d$vot_pre <- as.factor(prod_t_d$vot_pre)

prod_ppall <- prod_ppall %>% mutate(change = vot_pre - vot)
prod_ppall$prod_stim.y <- as.factor(prod_ppall$prod_stim.y)
prod_ppall_t <-  subset(prod_ppall, prod_stim.y == "level_2")
prod_ppall_d <-  subset(prod_ppall, prod_stim.y == "level_1")

prod_ppall_mean <- prod_ppall %>%
  group_by(Rec_Session_Id) %>%
  summarise_at(vars(change), list(name = mean))
prod_ppall_t_m <-  subset(prod_ppall_mean, prod_stim.y == "level_2")
prod_ppall_d_m <-  subset(prod_ppall_mean, prod_stim.y == "level_1")


ggplot(prod_ppall_t, aes (x=condition.x, y=change, color = condition.x))+
                          geom_violin(aes(group = condition.x)) +
                         geom_boxplot(width = 0.1)+
                         geom_point()

ggplot(prod_ppall_d, aes (x=condition.x, y=change, color = condition.x))+
  geom_violin(aes(group = condition.x)) +
  geom_boxplot(width = 0.1)+
  geom_point()
                        

prod_din <- subset(prod_all, Stimulus == "din")
prod_tin <- subset(prod_all, Stimulus == "tin")


#there is a 10 row difference between pre and post... so we are just going to cut 10 random rows from the data 

num_rows <- nrow(prod_post)

# Generate 10 random row indices
random_rows <- sample(num_rows, 10)

# Remove the selected rows from the data frame
prod_post_cut <- prod_post[-random_rows, ]

prod_bothSR <-  merge(prod_pre, pro)

#test duration differences
t.test(prod_post_cut$Speech_duration,prod_pre$Speech_duration)

mean(prod_post_cut$Speech_duration)
mean(prod_pre$Speech_duration)
                  


ggplot(data = prod_all, aes( x= Task_Name, y = Speech_duration, color = Task_Name))+
  geom_boxplot()


#check the distribution:
hist(prod_all$vot)

#try a model

mt300<- lmer(log(vot)~ Task_Name * bias * treatment * Stimulus + (1|Rec_Session_Id), data = prod_all)
summary(mt300) #nope

#try the thing with a poisson ---- doesn't converge
mt3<- glmer(vot~ Task_Name * bias * treatment * Stimulus + (1|Rec_Session_Id), family = poisson, data = prod_all)
summary(mt3)

mt1 <- rlmer(vot_norm ~ Task_Name * bias * treatment * Stimulus + (1|Rec_Session_Id), data = prod_all)
summary(mt1) #nope




mt5<- glmer.nb(vot~ Task_Name * condition * Stimulus + (1|Rec_Session_Id), data = prod_all)
summary(mt5) #singular fit, definitely the wrong model

#Eureka, it works!!!
mt4<- rlmer(vot~ Task_Name * condition * Stimulus + (1|Rec_Session_Id), data = prod_all)
summary(mt4)

emmt4<- emmeans(mt4,  ~ Task_Name *condition * Stimulus)
contrasts(emmt4)#, method = "pairwise")
pairs(emmt4, adjust = "fdr")

mt09u<- rlmer(vot~ Task_Name * condition * Stimulus + (1+Stimulus|Rec_Session_Id), data = prod_all)

mt2 <- rlmer(vot ~ Task_Name * bias * treatment * Stimulus + (1|Rec_Session_Id), data = prod_all)
summary(mt2)
#mt0 <- rlmer(vot ~ ((1+Task_Name|Rec_Session_Id)), data=prod_in)

#now try it with the normed vot and learn there's no difference between this and the actual model 
m_norm <- rlmer(vot_norm ~ Task_Name * bias * treatment * Stimulus + (1|Rec_Session_Id), data = prod_all)
summary(m_norm)

coefs <- fixef(mt4)
se <- sqrt(diag(vcov(mt4)))

# Compute z-scores and p-values
z <- coefs / se
p <- 2 * (1 - pnorm(abs(z)))

# Combine results into a data.frame
results <- data.frame(coef=coefs, se=se, z=z, p=p)
print(results)


# Perform the likelihood ratio test
lrtest(mt1)
summary(mt1)
report(mt1)
# Summarize the model fit
summary(mt1, se="boot", diag=FALSE)

#summary(mt1,vcov=vcov)
anova(mt1)
report(mt1)

emm4 <- emmeans(mt1,  ~ bias * treatment * Task_Name)
emm4pairs <- pairs(emm4, adjust = "fdr")






#plot the whole thing


prod_all<- prod_all %>% mutate(task = case_when(Task_Name == "Production_pre"~ "pre-test", Task_Name == "Production_Post" ~"post-test"))

levels <- c('pre-test', 'post-test')

prod_all <-  prod_all %>% mutate(treat = case_when(treatment == "interaction" ~ "speaking & listening", treatment == "control" ~ "listening"))

ggplot(data = prod_all, aes (x=task, y=vot, color = Stimulus))+
         geom_violin(aes(group = task))+
        geom_point(aes(group = task))+
        scale_x_discrete(limits = levels)+
        geom_boxplot(width = 0.2, aes(group = task))+
          facet_grid(treat ~ bias ~ Stimulus)+
  scale_color_manual(values = wes_palette("GrandBudapest1"))+
  theme_bw()+
  labs(fill ="produced stimulus" )+
  xlab(label = "Task")+
  ylab(label = "VOT in seconds")
  



