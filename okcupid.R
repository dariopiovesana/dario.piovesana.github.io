rm(list=ls())

library(corrplot)
library(VIM)
library(dplyr)
library(caret)
library(pROC)
library(randomForestSRC)
library(randomForest)
library(ROSE)
library(ROCR)
library(pROC)

train=read.csv("101.csv",stringsAsFactors = T)
test=read.csv("102.csv",stringsAsFactors = T)

# train1=read.csv("101.csv",stringsAsFactors = T, na.string=c("body_type_missing", "diet_missing", "drinks_missing",
# "drugs_missing", "ed_missing", "missing", "kids_missing", "pets_missing", "religion_missing",
# "sign_missing", "smokes_missing", "religion_mod_missing", "sign_mod_missing"))
# test1=read.csv("102.csv",stringsAsFactors = T, na.string=c("body_type_missing", "diet_missing", "drinks_missing",
#                                                           "drugs_missing", "ed_missing", "missing", "kids_missing", "pets_missing", "religion_missing",
#                                                           "sign_missing", "smokes_missing", "religion_mod_missing", "sign_mod_missing"))
test$Class = NA
n = nrow(train)
m = nrow(test)
combi = rbind(train,test)
train = combi[1:n,]
test = combi[(n+1):(n+m),]

# any(is.na(combi))
# which(is.na(combi))
# combi
# 
# aggr(combi,prop=F,combined=T,numbers=T,sortVars=T,sortCombs=T) #gli na sono quelli che ho aggiunto io

#trasformazioni delle variabili ( in verità non va trasformato e ci sono altri NA)
# combi$income=as.numeric(combi$income)

# attach(combi)
# table(asian)#nessun asiatico
# table(black)#nessuno black
# table(hispanic_latin)#nessuno
# table(indian)#nessuno
# table(middle_eastern)
# table(native_american)
# table(pacific_islander)
# table(white)

#le tolgo dal dataset
combi=combi[,-c(21:28)]
train = combi[1:n,]
test = combi[(n+1):(n+m),]


# table(combi$male)
# 0    1 
# 1977 3023 
# colnames(combi)[17]="Sex"
# combi$Sex=as.factor(combi$Sex)
# table(combi$Sex)#come prima
# levels(combi$Sex)=c('F','M')
# table(combi$Sex)
# F    M 
# 1977 3023 




# combi[,24:83]=sapply(combi[,24:83],factor)#non so perchè non traforma

risposte=combi%>% select_if(is.integer)%>% colnames()
combi[,risposte]=data.frame(sapply(combi[,risposte], as.factor))

combi$age<-as.integer(combi$age)
combi$height<-as.integer(combi$height)
combi$last_online<-as.integer(combi$last_online)
combi$essay_length<-as.numeric(combi$essay_length)
sapply(combi,"class")

train = combi[1:n,]
test = combi[(n+1):(n+m),]
table(train$Class)
#dataset sbilanciato
#18% stem

# table(combi$Class, combi$tech)[1,1]
# stem<-c()
# other<-c()
# for (i in 24:83){
#   a<-table(combi$Class, combi[,i])[1,1]
#   b<-table(combi$Class, combi[,i])[1,2]
#   c<-table(combi$Class, combi[,i])[2,1]
#   d<-table(combi$Class, combi[,i])[2,2]
#   e<-d/(c+d)
#   f<-b/(a+b)
#   stem<-c(stem,e)
#   other<-c(other,f)
# }
# stem<-round(stem, 2)
# other<-round(other,2)
# namecol<-colnames(combi[,24:83])
# t<-cbind(namecol, stem, other)
# t=as.data.frame(t)
# t$stem=as.numeric(t$stem)
# t$other=as.numeric(t$other)
# vet=t$stem==t$other
# which(vet==T)
# namecol[which(vet==T)]
#


 # body_type_missing
 # diet_missing
 # drinks_missing
 # drugs_missing
 # ed_missing
 # missing
 # kids_missing
 # pets_missing
 # religion_missing
 # sign_missing
 # smokes_missing
 # religion_mod_missing
 # sign_mod_missing



#per vedere quale variabile tra le dummy è più informativa
names.cont.vars=names(combi)[c(24:83)]

for(i in names.cont.vars)
{
  #outline=F non visualizza gli outliers nei boxplot (opzione per migliorare la visualizzazione)
  plot(train[,i]~train$Class, xlab="class", ylab=i, outline=F)
  cat ("Premere [invio] per continuare")
  readline()
}

table(train$school,train$Class)
862/(862+2409)
155/(155+574)
#poche sembrano essere informative
#analisi esplorativa

f=table(combi$Sex,combi$Class)
barplot(f,col=c("pink","blue"),
        legend=rownames(f))

table(combi$Sex)

par(mfrow=c(2,2))
NAMES <- names(train)[c(24:83)]
# "tech"       "technology" "math"       "computer"   "geek"       "science"    "fixing"    
# [8] "climbing"   "matrix"     "electronic"

for(i in NAMES)
{f=table(combi[,i],combi$Class)
barplot(f,col=c("pink","blue"),
        main=i,
        legend=rownames(f))

}


#bilanciamento
ctrl <- trainControl(method = "cv",
                     number = 10,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)
set.seed(123)
fit <- train(Class ~ ., data = train, 
             method = "treebag",
             nbagg = 50,
             metric = "ROC",
             trControl = ctrl,
             na.action=na.omit)

ctrl1 <- trainControl(method = "cv",
                     number = 10,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "down"
)
set.seed(123)
fit_di <- train(Class ~ ., data = train, 
                method = "treebag",
                nbagg = 50,
                metric = "ROC",
                trControl = ctrl1,
                na.action=na.omit)
ctrl$sampling="up"
set.seed(123)
fit_ui <- train(Class ~ ., data = train, 
                method = "treebag",
                nbagg = 50,
                metric = "ROC",
                trControl = ctrl1,
                na.action=na.omit)


models_in <- list(original = fit,
                  down = fit_di,
                  up = fit_ui)
res_in <- resamples(models_in)
bwplot(res_in, metric = "ROC")
phat = predict(fit, test, type = "prob")[,"stem"]
phat_di = predict(fit_di, test,  type = "prob")[,"stem"]
phat_ui = predict(fit_ui, test,  type = "prob")[,"stem"]

write.table(file="myokcupidnNA.txt", phat, row.names = FALSE, col.names = FALSE)
write.table(file="myokcupiddNA.txt", phat_di, row.names = FALSE, col.names = FALSE)
write.table(file="myokcupiduNA.txt", phat_ui, row.names = FALSE, col.names = FALSE)

#############################
###prova con alcune
#prova con alcune


var=subset(combi,select=c(27,34,38,43,44,53,52,57,58,59,66,65,70,75,73,77,80,83))

combi2=combi[,-c(24:83)]
combi3=cbind(combi2,var)

train = combi3[1:n,]
test = combi3[(n+1):(n+m),]


#bilanciamento
ctrl <- trainControl(method = "cv",
                     number = 10,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)
set.seed(123)
fit <- train(Class ~ ., data = train, 
             method = "treebag",
             nbagg = 50,
             metric = "ROC",
             trControl = ctrl)

ctrl1 <- trainControl(method = "cv",
                      number = 10,
                      classProbs = TRUE,
                      summaryFunction = twoClassSummary,
                      sampling = "down"
)
set.seed(123)
fit_di <- train(Class ~ ., data = train, 
                method = "treebag",
                nbagg = 50,
                metric = "ROC",
                trControl = ctrl1)
ctrl$sampling="up"
set.seed(123)
fit_ui <- train(Class ~ ., data = train, 
                method = "treebag",
                nbagg = 50,
                metric = "ROC",
                trControl = ctrl1)


models_in <- list(original = fit,
                  down = fit_di,
                  up = fit_ui)
res_in <- resamples(models_in)
bwplot(res_in, metric = "ROC")
phat = predict(fit, test, type = "prob")[,"stem"]
phat_di = predict(fit_di, test,  type = "prob")[,"stem"]
phat_ui = predict(fit_ui, test,  type = "prob")[,"stem"]

write.table(file="myokcupidn1.txt", phat, row.names = FALSE, col.names = FALSE)
write.table(file="myokcupidd1.txt", phat_di, row.names = FALSE, col.names = FALSE)
write.table(file="myokcupidu1.txt", phat_ui, row.names = FALSE, col.names = FALSE)


#rf
# previsioni<-matrix(NA,1000,100)
# for (i in 1:100){
# train.bil<-ovun.sample(Class~., train, method = "both", 600, p=0.5)
# train.b<-train.bil$data
# rf<-rfsrc(Class~., train.b, 5000)
# prev<-predict.rfsrc(rf, test)
# pred<-prev$yvar
# previsioni[,i]<-prev[i]
# }
# prev$class
#non va bene
# rf.omit<-rfsrc(Class~., train, 1000, na.action = "na.omit")
# prev.omit<-predict.rfsrc(rf, test2, na.action= "na.omit")
# rf.<-rfsrc(Class~., train, 1000)
# prev.<-predict.rfsrc(rf, test)



#rf
b<-matrix(NA, 100, 82)
for(i in 1:100){
train.bil<-ovun.sample(Class~., train, method = "both", 600, p=0.5)
train.b<-train.bil$data
m0<-randomForest(Class~., train.b)
w<-varImp(m0)
e<-w$Overall
b[i,]<-e
}
#la matrice b mi permette di selezionare i parametri
colnames(combi)
combimenoclass<-combi[,-23]
colnames(b)<-colnames(combimenoclass)

#seleziono solo le colonne maggiori di 2 in media
media<-c()
for (i in 1:82){
  media[i]<-mean(b[,i])
}
var<-matrix(media, 1, 82)
colnames(var)<-colnames(b)
var<-as.data.frame(var)
nomivar<-select_if(var, var>1)
nomicol<-colnames(nomivar)
combi1<-combi[,nomicol]
combi1$Class<-combi$Class

n = nrow(train)
m = nrow(test)
train1<-combi1[1:n,]
test1<-combi1[(n+1):(n+m),]

#devo sostituire la classe kosher, quella halal
table(combi1$diet)
table(combi1$diet, combi1$Class)
combi1$diet[which(combi1$diet=="mostly_halal")]<-"mostly_other"
combi1$diet[which(combi1$diet=="mostly_kosher")]<-"mostly_other"
combi1$diet[which(combi1$diet=="strictly_kosher")]<-"strictly_other"
combi1$diet[which(combi1$diet=="kosher")]<-"other"

table(combi1$education)
table(combi1$education, combi1$Class)
combi1$education[which(combi1$education=="law_school")]<-"graduated_from_law_school"
combi1$education[which(combi1$education=="working_on_med_school")]<-"graduated_from_med_school"
combi1$education[which(combi1$education=="working_on_law_school")]<-"graduated_from_law_school"
combi1$education[which(combi1$education=="working_on_high_school")]<-"graduated_from_high_school"
combi1$education[which(combi1$education=="space_camp")]<-"dropped_out_of_space_camp"
combi1$education[which(combi1$education=="two_year_college")]<-"graduated_from_two_year_college"
combi1$education[which(combi1$education=="ph_d_program")]<-"graduated_from_ph_d_program"
combi1$education[which(combi1$education=="high_school")]<-"graduated_from_high_school"
combi1$education[which(combi1$education=="masters_program")]<-"graduated_from_masters_program"
combi1$education[which(combi1$education=="dropped_out_of_high_school")]<-"graduated_from_high_school"
combi1$education[which(combi1$education=="dropped_out_of_law_school")]<-"graduated_from_law_school"
combi1$education[which(combi1$education=="dropped_out_of_masters_program")]<-"graduated_from_masters_program"
combi1$education[which(combi1$education=="dropped_out_of_ph_d_program")]<-"graduated_from_ph_d_program"

table(combi1$offspring)
table(combi1$offspring, combi1$Class)
combi1$offspring[which(combi1$offspring=="has_kids_and_wants_more")]<-"has_a_kid_and_might_want_more"
combi1$offspring[which(combi1$offspring=="has_a_kid_and_wants_more")]<-"has_a_kid_and_might_want_more"
combi1$offspring[which(combi1$offspring=="has_kids_and_might_want_more")]<-"has_a_kid_and_might_want_more"
combi1$offspring[which(combi1$offspring=="wants_kids")]<-"doesnt_have_kids_but_wants_them"
combi1$offspring[which(combi1$offspring=="might_want_kids")]<-"doesnt_have_kids_but_might_want_them"

table(combi1$pets)
table(combi1$pets, combi1$Class)
combi1$pets[which(combi1$pets=="dislikes_cats")]<-"dislikes_dogs_and_dislikes_cats"
combi1$pets[which(combi1$pets=="dislikes_dogs")]<-"dislikes_dogs_and_dislikes_cats"
combi1$pets[which(combi1$pets=="dislikes_dogs_and_has_cats")]<-"dislikes_dogs_and_likes_cats"

table(combi1$religion)
table(combi1$religion, combi1$Class)
combi1$religion[which(combi1$religion=="islam")]<-"hinduism"

table(combi1$where_town)
table(combi1$where_town, combi1$Class)
table(test1$where_town)
combi1$where_town[which(combi1$where_town=="albany")]<-"menlo_park"
combi1$where_town[which(combi1$where_town=="el_sobrante")]<-"benicia"
combi1$where_town[which(combi1$where_town=="green_brae")]<-"benicia"
combi1$where_town[which(combi1$where_town=="half_moon_bay")]<-"benicia"
combi1$where_town[which(combi1$where_town=="burlingame")]<-"castro_valley"
combi1$where_town[which(combi1$where_town=="corte_madera")]<-"stanford"
combi1$where_town[which(combi1$where_town=="fairfax")]<-"stanford"
combi1$where_town[which(combi1$where_town=="fremont")]<-"stanford"
combi1$where_town[which(combi1$where_town=="millbrae")]<-"stanford"
combi1$where_town[which(combi1$where_town=="sausalito")]<-"stanford"
combi1$where_town[which(combi1$where_town=="hercules")]<-"other"
combi1$where_town[which(combi1$where_town=="lafayette")]<-"other"
combi1$where_town[which(combi1$where_town=="larkspur")]<-"other"
combi1$where_town[which(combi1$where_town=="mill_valley")]<-"other"
combi1$where_town[which(combi1$where_town=="millbrae")]<-"other"
combi1$where_town[which(combi1$where_town=="moraga")]<-"other"
combi1$where_town[which(combi1$where_town=="orinda")]<-"other"
combi1$where_town[which(combi1$where_town=="pinole")]<-"other"
combi1$where_town[which(combi1$where_town=="san_anselmo")]<-"other"
combi1$where_town[which(combi1$where_town=="san_bruno")]<-"stanford"
combi1$where_town[which(combi1$where_town=="san_carlos")]<-"other"
combi1$where_town[which(combi1$where_town=="san_lorenzo")]<-"other"
combi1$where_town[which(combi1$where_town=="san_pablo")]<-"other"
combi1$where_town[which(combi1$where_town=="sausalito")]<-"other"

table(combi$body_type)
table(combi$body_type, combi$Class)
combi$body_type[which(combi$body_type=="jacked")]<-"fit"
combi$body_type[which(combi$body_type=="rather_not_say")]<-"full_figured"
combi$body_type[which(combi$body_type=="used_up")]<-"skinny"


table(combi$income)
table(combi$income, combi$Class)
combi$income[which(combi$income=="inc500000")]<-"inc1000000"
combi$income[which(combi$income=="inc250000")]<-"inc1000000"

train1<-combi1[1:n,]
test1<-combi1[(n+1):(n+m),]


#glm
# matrice.previsioni<-matrix(NA, 1000, 100)
# for(i in 1:100)
train.bil<-ovun.sample(Class~., train1, method = "both", 600, p=0.5)
train.b<-train.bil$data
m1<-glm(Class~., family = binomial ,train.b)
prev<-predict.glm(m1, test1, type = "response")
pred<-ifelse(prev>0.5, "stem", "other")



#salvo la previsione
write.table(file="okcupido.txt", pred, row.names = FALSE, col.names = FALSE)

#altro modo
#rf
ctrl1 <- trainControl(method = "cv",
                      number = 10,
                      
                      classProbs = TRUE,
                      summaryFunction = twoClassSummary,
                      sampling = "down"
)

fit_di <- train(Class ~ ., data = train, 
                ntree=100,
                method = "rf",
                
                localImp=T,
                metric = "ROC",
                trControl = ctrl1)
fit_di$finalModel
varImp(fit_di)


plot(varImp(fit_di))
w=varImp(fit_di)
w$importance
r=w$importance[1]
t=t(r)
t=as.data.frame(t)
u=select_if(t,t>20)
w<-colnames(u)



#cambio variabili
combi$body_typeathletic<-combi$body_type=="athletic"
combi$body_typefull_figured<-combi$body_type=="full_figured"
combi$body_typejacked<-combi$body_type=="jacked"
combi$body_typeskinny<-combi$body_type=="skinny"
combi$body_typethin<-combi$body_type=="thin"
combi$body_typeused_up<-combi$body_type=="used_up"
combi$dietmostly_other<-combi$diet=="mostly_other"
combi$dietmostly_vegan<-combi$diet=="mostly_vegan"
combi$dietmostly_vegetarian<-combi$diet=="mostly_vegetarian"
combi$dietstrictly_vegan<-combi$diet=="strictly_vegan"
combi$dietvegetarian<-combi$diet=="vegetarian"
combi$drinksoften<-combi$drinks=="often"
combi$educationdropped_out_of_high_school<-combi$education=="dropped_out_of_high_school"
combi$educationed_missing<-combi$education=="ed_missing"
combi$educationgraduated_from_college_university<-combi$education=="graduated_from_college_university"
combi$educationgraduated_from_law_school<-combi$education=="graduated_from_law_school"
combi$educationgraduated_from_masters_program<-combi$education=="graduated_from_masters_program"
combi$educationgraduated_from_ph_d_program<-combi$education=="graduated_from_ph_d_program"
combi$educationworking_on_college_university<-combi$education=="working_on_college_university"
combi$educationworking_on_masters_program<-combi$education=="working_on_masters_program"
combi$educationworking_on_two_year_college<-combi$education=="working_on_two_year_college"
combi$incomeinc20000<-combi$income=="inc20000"
combi$incomeinc250000<-combi$income=="inc250000"
combi$incomeinc30000<-combi$income=="inc30000"
combi$incomeinc80000<-combi$income=="inc80000"
combi$offspringkids_missing<-combi$offspring=="kids_missing"
combi$offspringmight_want_kids<-combi$offspring=="might_want_kids"
combi$orientationgay<-combi$orientation=="gay"
combi$petslikes_dogs_and_dislikes_cats<-combi$pets=="likes_dogs_and_dislikes_cats"
combi$petslikes_dogs_and_has_cats<-combi$pets=="likes_dogs_and_has_cats"
combi$petspets_missing<-combi$pets=="pets_missing"
combi$religionatheism<-combi$religion=="atheism"
combi$religioncatholicism<-combi$religion=="catholicism"
combi$religionother<-combi$religion=="other"
combi$religionreligion_missing<-combi$religion=="religion_missing"
combi$signcancer<-combi$sign=="cancer"
combi$signgemini<-combi$sign=="gemini"
combi$signpisces<-combi$sign=="pisces"
combi$signsagittarius<-combi$sign=="sagittarius"
combi$signsign_missing<-combi$sign=="sign_missing"
combi$smokessometimes<-combi$smokes=="sometimes"
combi$smokeswhen_drinking<-combi$smokes=="when_drinking"
combi$smokesyes<-combi$smokes=="yes"
combi$statusseeing_someone<-combi$status=="seeing_someone"
combi$statussingle<-combi$status=="single"
combi$where_townbelmont<-combi$where_town=="belmont"
combi$where_townburlingame<-combi$where_town=="burlingame"
combi$where_townel_cerrito<-combi$where_town=="el_cerrito"
combi$where_townemeryville<-combi$where_town=="emeryville"
combi$where_townfremont<-combi$where_town=="fremont"
combi$where_townoakland<-combi$where_town=="oakland"
combi$where_townother<-combi$where_town=="other"
combi$where_townpacifica<-combi$where_town=="pacifica"
combi$where_townpalo_alto<-combi$where_town=="palo_alto"
combi$where_townpleasant_hill<-combi$where_town=="pleasant_hill"
combi$where_townredwood_city<-combi$where_town=="redwood_city"
combi$where_townsan_francisco<-combi$where_town=="san_francisco"
combi$where_townsan_leandro<-combi$where_town=="san_leandro"
combi$where_townsan_mateo<-combi$where_town=="san_mateo"
combi$where_townsan_rafael<-combi$where_town=="san_rafael"
combi$religion_modiferbut_not_too_serious_about_it<-combi$religion_modifer=="but_not_too_serious_about_it"
combi$sign_modiferbut_it_doesnt_matter<-combi$sign_modifer=="but_it_doesnt_matter"

train<-combi[1:n,]
test<-combi[(n+1):(n+m),]

#creo la formula
eandiamo<-as.formula(Class ~ age+body_typeathletic+body_typefull_figured+body_typejacked+body_typeskinny+body_typethin+body_typeused_up+dietmostly_other+dietmostly_vegan+dietmostly_vegetarian+dietstrictly_vegan+dietvegetarian+drinksoften+educationdropped_out_of_high_school+educationed_missing+educationgraduated_from_college_university+educationgraduated_from_law_school+educationgraduated_from_masters_program+educationgraduated_from_ph_d_program+educationworking_on_college_university+
                       educationworking_on_masters_program+educationworking_on_two_year_college+height+incomeinc20000+incomeinc250000+incomeinc30000+incomeinc80000+offspringkids_missing+offspringmight_want_kids+orientationgay+petslikes_dogs_and_dislikes_cats+petslikes_dogs_and_has_cats+petspets_missing+
                       religionatheism+religioncatholicism+religionother+religionreligion_missing+signcancer+signgemini+signpisces+signsagittarius+signsign_missing+smokessometimes+smokeswhen_drinking+smokesyes+statusseeing_someone+statussingle+male+where_townbelmont+where_townburlingame+where_townel_cerrito+
                       where_townemeryville+where_townfremont+where_townoakland+where_townother+where_townpacifica+where_townpalo_alto+where_townpleasant_hill+where_townredwood_city+where_townsan_francisco+where_townsan_leandro+where_townsan_mateo+where_townsan_rafael+religion_modiferbut_not_too_serious_about_it+sign_modiferbut_it_doesnt_matter+essay_link+essay_length+
                       tech+technology+math+computer+science+matrix+electronic+internet+skiing+solving+board+brain+firefly+company+build+hobbies+recent+awesome+law+lol+justice+political+artist+student+tattoos+ipod+native+baseball+major+lover+school+loyal)

eandiamo2<-as.formula(Class ~ age+body_typeathletic+body_typefull_figured+body_typejacked+body_typeskinny+body_typethin+body_typeused_up+dietmostly_other+dietmostly_vegan+dietmostly_vegetarian+dietstrictly_vegan+dietvegetarian+drinksoften+educationdropped_out_of_high_school+educationed_missing+educationgraduated_from_college_university+educationgraduated_from_law_school+educationgraduated_from_masters_program+educationgraduated_from_ph_d_program+educationworking_on_college_university+
                       educationworking_on_masters_program+educationworking_on_two_year_college+height+incomeinc20000+incomeinc250000+incomeinc30000+incomeinc80000+offspringkids_missing+offspringmight_want_kids+orientationgay+petslikes_dogs_and_dislikes_cats+petslikes_dogs_and_has_cats+petspets_missing+
                       religionatheism+religioncatholicism+religionother+religionreligion_missing+signsign_missing+smokessometimes+smokeswhen_drinking+smokesyes+statusseeing_someone+statussingle+male+where_townbelmont+where_townburlingame+where_townel_cerrito+
                       where_townemeryville+where_townfremont+where_townoakland+where_townother+where_townpacifica+where_townpalo_alto+where_townpleasant_hill+where_townredwood_city+where_townsan_francisco+where_townsan_leandro+where_townsan_mateo+where_townsan_rafael+religion_modiferbut_not_too_serious_about_it+sign_modiferbut_it_doesnt_matter+essay_link+essay_length+
                       tech+technology+math+computer+science+matrix+electronic+internet+skiing+solving+board+brain+firefly+company+build+hobbies+recent+awesome+law+lol+justice+political+artist+student+tattoos+ipod+native+baseball+major+lover+school+loyal)


#glm 


ctrl1 <- trainControl(method = "cv",
                      number = 10,
                      classProbs = TRUE,
                      summaryFunction = twoClassSummary,
                      sampling = "down"
)
set.seed(123)
fit_di <- train(eandiamo, data = train, 
                method = "glm",
                
                metric = "ROC",
                trControl = ctrl1)
phat_eandiamo = predict(fit_di, test,  type = "prob")[,"stem"]
write.table(file="rfglmvarsel.txt", phat_eandiamo, row.names = FALSE, col.names = FALSE)

#glm 

# ctrl1 <- trainControl(method = "cv",
#                       number = 10,
#                       classProbs = TRUE,
#                       summaryFunction = twoClassSummary,
#                       sampling = "down"
# )
# set.seed(234)
# fit_di <- train(eandiamo2, data = train, 
#                 method = "glm",
#                 
#                 metric = "ROC",
#                 trControl = ctrl1)
# phat_eandiamo = predict(fit_di, test,  type = "prob")[,"stem"]
# write.table(file="rfglmvarsel1.txt", phat_eandiamo, row.names = FALSE, col.names = FALSE)

#ciclo

mat<-matrix(NA, 100, 1000)
set.seed(1206*i)
ctrl1 <- trainControl(method = "cv",
                      number = 10,
                      classProbs = TRUE,
                      summaryFunction = twoClassSummary,
                      sampling = "down")
for (i in 1:100){
fit_di <- train(eandiamo, data = train, 
                method = "glm",
                metric = "ROC",
                trControl = ctrl1)
phat_eandiamo = predict(fit_di, test,  type = "prob")[,"stem"]
mat[i,]<-phat_eandiamo
}
previsioni<-c()
for (i in 1:1000) {
  previsioni[i]<-median(mat[,i])
}
write.table(file="prevfinale.txt", previsioni, row.names = FALSE, col.names = FALSE)

#ciclo ovun.sample
mat2<-matrix(NA, 100, 1000)
for (i in 1:100){
train.bil<-ovun.sample(eandiamo, train, method = "both", 1000, p=0.5)
train.b<-train.bil$data
ctrl2 <- trainControl(method = "cv",
                      number = 10,
                      classProbs = TRUE,
                      summaryFunction = twoClassSummary)
fit_di2 <- train(eandiamo, data = train.b, 
                  method = "glm",
                  metric = "ROC",
                  trControl = ctrl2)
  phat_eandiamo = predict(fit_di2, test,  type = "prob")[,"stem"]
  mat2[i,]<-phat_eandiamo
}
previsioni2<-c()
for (i in 1:1000) {
  previsioni2[i]<-median(mat2[,i])
}
previsioni2<-round(previsioni2, 10)
write.table(file="rfglmvarselcicloovun.txt", previsioni3, row.names = FALSE, col.names = FALSE)


#ciclo ovun.sample interno

mat3<-matrix(NA, 100, 1000)
smotest <- list(name = "SMOTE with more neighbors!",
                func = function (x, y) {
                  library(DMwR)
                  dat <- if (is.data.frame(x)) x else as.data.frame(x)
                  dat$.y <- y
                  dat <- SMOTE(.y ~ ., perc.over = 100, data = dat, k = 5)
                  list(x = dat[, !grepl(".y", colnames(dat), fixed = TRUE)], 
                       y = dat$.y)
                },
                first = TRUE)
ctrl3 <- trainControl(method = "cv",
                      number = 10,
                      classProbs = TRUE,
                      summaryFunction = twoClassSummary,
                      sampling = smotest)
for (i in 1:100){
  set.seed(324*i)
  fit_di3 <- train(eandiamo, data = train, 
                   method = "glm",
                   metric = "ROC",
                   trControl = ctrl3)
  phat_eandiamo = predict(fit_di3, test,  type = "prob")[,"stem"]
  mat3[i,]<-phat_eandiamo
}
previsioni3<-c()
for (i in 1:1000) {
  previsioni3[i]<-median(mat3[,i])
}
write.table(file="rfglmsmoooooooooooote.txt", previsioni3, row.names = FALSE, col.names = FALSE)

#secondo tentativo ovun.sample interno alla cv
matrice.auc<-matrix(NA, 100, 20)
matrice<-array(NA, c(20, 100, 1000))
matr.prova.prev<-matrix(NA, 20,1001)
vett.prova.auc<-c()
matr.previsioni<-matrix(NA, 100, 1000)
matr.prova.prev3<-matrix(NA, 20,1001)
for (i in 1:100){
  for (j in 1:20){
    id<-sample(1:4000, 4000,replace = F)
    test.cv<-train[id[(200*(j-1)+1):(200*j)],]
    train.cv<-train[-id[(200*(j-1)+1):(200*j)],]
    train.bil<-ovun.sample(eandiamo, train, method = "both", 1000, p=0.5)$data
    fit_di4 <- glm(eandiamo, data = train.bil, family = binomial)
    percentuale = predict(fit_di4, test.cv,  type = "response")
    test.cv$percentuale<-percentuale
    cr<-roc(Class~percentuale, test.cv)$auc
    cr.auc<-cr
    vett.prova.auc<-c(vett.prova.auc,cr)
    #matrice.auc[i,j]<-cr
    phat_eandiamo = round(predict(fit_di4, test, type = "response"), 10)
    matr.prova.prev[j,]<-phat_eandiamo
    matr.prova.prev2<-cbind(matr.prova.prev[j,], vett.prova.auc, byrow=F)
    matr.prova.prev3<-matr.prova.prev2[which(matr.prova.prev2[,1001]>0.8),]
    prev<-c()
    for (k in 1:1000){
      prev[k]<-median(matr.prova.prev2[which(matr.prova.prev2[,1001]>0.8),])
    }
    #matrice[j,i,]<-phat_eandiamo
  }
  matr.previsioni[i,]<-prev
}
prev2<-c()
for (i in 1:1000){
  prev2[i]<-median(matr.previsioni[,i])
}
write.table(file="glmcv.txt", prev2, row.names = FALSE, col.names = FALSE)


matrice.aucc<-as.data.frame(matrice.auc)
sum(matrice.aucc>0.8)#1014
matrice.new<-matrix(NA, 1014, 1000)
matrice.new2<-array(NA, c(20, 100, 1000))
matrice.new2<-matrice

matrice.aucc>0.8
head(matrice.new2[1,2,])
head(matrice.new2[2,1,])
head(matrice.new[1,])
head(matrice[,1,])

matrice.aucc<-t(matrice.aucc)
matrice.completa<-array(NA, c(20,100,1001))
for(i in 1:20){
  for(j in 1:100){
    for(k in 1:1000){
      matrice.completa[i,j,k]<-matrice.new2[i,j,k]
    }
  }
}
for(i in 1:20){
  for(j in 1:100){
matrice.completa[i,j,1001]<-matrice.aucc[i,j]
  }
}
sum(is.na(matrice.completa))

  X1<-matrix(NA, 100,1001)
  X1<-matrice.completa[1,,]
  X2<-matrix(NA, 100,1001)
  X2<-matrice.completa[2,,]
  X3<-matrix(NA, 100,1001)
  X3<-matrice.completa[3,,]
  X4<-matrix(NA, 100,1001)
  X4<-matrice.completa[4,,]
  X5<-matrix(NA, 100,1001)
  X5<-matrice.completa[5,,]
  X6<-matrix(NA, 100,1001)
  X6<-matrice.completa[6,,]
  X7<-matrix(NA, 100,1001)
  X7<-matrice.completa[7,,]
  X8<-matrix(NA, 100,1001)
  X8<-matrice.completa[8,,]
  X9<-matrix(NA, 100,1001)
  X9<-matrice.completa[9,,]
  X10<-matrix(NA, 100,1001)
  X10<-matrice.completa[10,,]
  X11<-matrix(NA, 100,1001)
  X11<-matrice.completa[11,,]
  X12<-matrix(NA, 100,1001)
  X12<-matrice.completa[12,,]
  X13<-matrix(NA, 100,1001)
  X13<-matrice.completa[13,,]
  X14<-matrix(NA, 100,1001)
  X14<-matrice.completa[14,,]
  X15<-matrix(NA, 100,1001)
  X15<-matrice.completa[15,,]
  X16<-matrix(NA, 100,1001)
  X16<-matrice.completa[16,,]
  X17<-matrix(NA, 100,1001)
  X17<-matrice.completa[17,,]
  X18<-matrix(NA, 100,1001)
  X18<-matrice.completa[18,,]
  X19<-matrix(NA, 100,1001)
  X19<-matrice.completa[19,,]
  X20<-matrix(NA, 100,1001)
  X20<-matrice.completa[20,,]

media1<-c()
for (i in 1:1000){
vett1<-mean(X1[which(X1[,1001]>0.8),i])
media1<-c(media1, vett1)
}

media2<-c()
for (i in 1:1000){
  vett2<-mean(X2[which(X2[,1001]>0.8),i])
  media2<-c(media2, vett2)
}

media3<-c()
for (i in 1:1000){
  vett3<-mean(X3[which(X3[,1001]>0.8),i])
  media3<-c(media3, vett3)
}

media4<-c()
for (i in 1:1000){
  vett4<-mean(X4[which(X4[,1001]>0.8),i])
  media4<-c(media4, vett4)
}

media5<-c()
for (i in 1:1000){
  vett5<-mean(X5[which(X5[,1001]>0.8),i])
  media5<-c(media5, vett5)
}

media6<-c()
for (i in 1:1000){
  vett6<-mean(X6[which(X6[,1001]>0.8),i])
  media6<-c(media6, vett6)
}

media7<-c()
for (i in 1:1000){
  vett7<-mean(X7[which(X7[,1001]>0.8),i])
  media7<-c(media7, vett7)
}

media8<-c()
for (i in 1:1000){
  vett8<-mean(X8[which(X8[,1001]>0.8),i])
  media8<-c(media8, vett8)
}

media9<-c()
for (i in 1:1000){
  vett9<-mean(X9[which(X9[,1001]>0.8),i])
  media9<-c(media9, vett9)
}

media10<-c()
for (i in 1:1000){
  vett10<-mean(X10[which(X10[,1001]>0.8),i])
  media10<-c(media10, vett10)
}

media11<-c()
for (i in 1:1000){
  vett11<-mean(X11[which(X11[,1001]>0.8),i])
  media11<-c(media11, vett11)
}

media12<-c()
for (i in 1:1000){
  vett12<-mean(X12[which(X12[,1001]>0.8),i])
  media12<-c(media12, vett12)
}

media13<-c()
for (i in 1:1000){
  vett13<-mean(X13[which(X13[,1001]>0.8),i])
  media13<-c(media13, vett13)
}

media14<-c()
for (i in 1:1000){
  vett14<-mean(X14[which(X14[,1001]>0.8),i])
  media14<-c(media14, vett14)
}

media15<-c()
for (i in 1:1000){
  vett15<-mean(X15[which(X15[,1001]>0.8),i])
  media15<-c(media15, vett15)
}

media16<-c()
for (i in 1:1000){
  vett16<-mean(X16[which(X16[,1001]>0.8),i])
  media16<-c(media16, vett16)
}

media17<-c()
for (i in 1:1000){
  vett17<-mean(X17[which(X17[,1001]>0.8),i])
  media17<-c(media17, vett17)
}

media18<-c()
for (i in 1:1000){
  vett18<-mean(X18[which(X18[,1001]>0.8),i])
  media18<-c(media18, vett18)
}

media19<-c()
for (i in 1:1000){
  vett19<-mean(X19[which(X19[,1001]>0.8),i])
  media19<-c(media19, vett19)
}

media20<-c()
for (i in 1:1000){
  vett20<-mean(X20[which(X20[,1001]>0.8),i])
  media20<-c(media20, vett20)
}

vett.medie<-matrix(NA, 20, 1000)
  vett.medie[1,]<-media1
  vett.medie[2,]<-media2
  vett.medie[3,]<-media3
  vett.medie[4,]<-media4
  vett.medie[5,]<-media5
  vett.medie[6,]<-media6
  vett.medie[7,]<-media7
  vett.medie[8,]<-media8
  vett.medie[9,]<-media9
  vett.medie[10,]<-media10
  vett.medie[11,]<-media11
  vett.medie[12,]<-media12
  vett.medie[13,]<-media13
  vett.medie[14,]<-media14
  vett.medie[15,]<-media15
  vett.medie[16,]<-media16
  vett.medie[17,]<-media17
  vett.medie[18,]<-media18
  vett.medie[19,]<-media19
  vett.medie[20,]<-media20

previs<-c()  
for( i in 1:1000){
 mediana<-median(vett.medie[,i]) 
 previs<-c(previs,mediana)
}
previs<-(previs-1)*(-1)
write.table(file="glm.txt", previs, row.names = FALSE, col.names = FALSE)