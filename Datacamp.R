train_in<-read.csv("D:/Salle2023/20200605_J12/traininginputs.csv")
train_out<-read.csv("D:/Salle2023/20200605_J12/trainingoutput.csv")

library(dplyr)

str(train_in)
summary(train_in)

train_f<-left_join(train_out,train_in, by="PROC_TRACEINFO")

str(train_f)

table(train_f$OP100_Capuchon_insertion_mesure,train_f$Binar.OP130_Resultat_Global_v,exclude = NULL)

train_f<- select(train_f,-OP100_Capuchon_insertion_mesure)

summary(train_f)


library(FactoMineR)

resacp<-PCA(train_f[,2:13])

table(train_f$OP120_Rodage_U_mesure_value)
table(train_f$OP090_SnapRingPeakForce_value)
table(train_f$OP090_SnapRingPeakForce_value,train_f$Binar.OP130_Resultat_Global_v)

sum(which(train_f$OP090_SnapRingFinalStroke_valu=0))

tnp<-table(train_f$OP120_Rodage_U_mesure_value,train_f$Binar.OP130_Resultat_Global_v)
(tnp[,2]/tnp[,1])*100

tnp2<-table(train_f$Binar.OP130_Resultat_Global_v)
tnp_p<-train_f$Binar.OP130_Resultat_Global_v*100/sum(train_f$Binar.OP130_Resultat_Global_v)

prop.table(train_f$Binar.OP130_Resultat_Global_v)

prop.table(train_f$Binar.OP130_Resultat_Global_v)
  
(tnp2[,2]/tnp2[,1])*100

#OP090_SnapRingPeakForce_value

#suppression des <120 en visage
train_fclean<-train_f[-which(train_f$OP070_V_1_angle_value<120),]
#suppression des 0 en SnapRingPeakForce
train_fclean<-train_f[-which(train_f$OP090_SnapRingPeakForce_value=0),]


plot(train_f[,1]~train_f$Binar.OP130_Resultat_Global_v)

library(randomForest)

train_f_tn <- train_f %>% sample_frac(0.8)
train_f_tt <- anti_join(train_f, train_f_tn)




