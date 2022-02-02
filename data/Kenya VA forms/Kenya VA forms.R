library(readstata13)
library(dplyr)

data0 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/allVA.dta")
data1 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/Coded_VA_childtables_legacy.dta")

data2 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/Coded_VA_symptoms_lastillness1_f1.dta")
data5 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/Coded_VA_treatment_healthrecords_f1.dta")

data3 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/Coded_VA_symptoms_lastillness1_f2.dta")
data4 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/Coded_VA_symptoms_lastillness2_f2.dta")
data6 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/Coded_VA_tretreatment_healthrecords_f2.dta")

data7 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/Coded_VA_uploadva_v7pt1.dta")
data8 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/Coded_VA_uploadva_v7pt2.dta")

data9 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/Coded_VA_uploadvachild_v8pt1.dta")
data10 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/Coded_VA_uploadvachild_v8pt2.dta")

data11 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/DSSVA_CLEAN_f1_symptoms_lastillness1.dta")
data12 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/DSSVA_CLEAN_f1_symptoms_lastillness2.dta")

data13 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/DSSVA_CLEAN_f2f3_f2f3_Previous_medical_condition1.dta")
data14 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/DSSVA_CLEAN_f2f3_Previous_medical_condition.dta")

data15 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/DSSVA_CLEAN_Treatment_Operation.dta")
data16 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/DSSVA_CLEAN_Treatment_Type.dta")

data17 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/DSSVA_NEW_CLEAN_f1_symptoms_lastillness.dta")
data18 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/DSSVA_NEW_CLEAN_f1_symptoms_lastillness1.dta")

data19 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/DSSVA_NEW_CLEAN_f2_symptoms_lastillness1.dta")
data20 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/DSSVA_NEW_CLEAN_f2_symptoms_lastillness2.dta")

data21 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/DSSVA_NEW_CLEAN_f2f3_Previous_medical_condition.dta")
data22 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/DSSVA_NEW_CLEAN_f2f3_Previous_medical_condition1.dta")

data23 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/DSSVA_NEW_CLEAN_Treatment_Type.dta")

data24 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/VA_legacy.dta")

data25 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/WHO_2007FM1.dta")
data26 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/WHO_2007FM2.dta")

data27 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/WHO2010_FORM1.dta")
data28 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/WHO2010_FORM2.dta")

data29 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/WHO2012_FORM1.dta")
data30 <- read.dta13("I:/Geomed/CVD Project Management/VIDA/Data Use and Management/VA/Kenya VA forms/WHO2012_FORM2.dta")

file.names <- c(rep("allVA", ncol(data0)),
                rep("Coded_VA_childtables_legacy", ncol(data1)),
                rep("Coded_VA_symptoms_lastillness1_f1", ncol(data2)),
                rep("Coded_VA_symptoms_lastillness1_f2", ncol(data3)),
                rep("Coded_VA_symptoms_lastillness2_f2", ncol(data4)),
                rep("Coded_VA_treatment_healthrecords_f1", ncol(data5)),
                rep("Coded_VA_tretreatment_healthrecords_f2", ncol(data6)),
                rep("Coded_VA_uploadva_v7pt1", ncol(data7)),
                rep("Coded_VA_uploadva_v7pt2", ncol(data8)),
                rep("Coded_VA_uploadvachild_v8pt1", ncol(data9)),
                rep("Coded_VA_uploadvachild_v8pt2", ncol(data10)),
                rep("DSSVA_CLEAN_f1_symptoms_lastillness1", ncol(data11)),
                rep("DSSVA_CLEAN_f1_symptoms_lastillness2", ncol(data12)),
                rep("DSSVA_CLEAN_f2f3_f2f3_Previous_medical_condition1", ncol(data13)),
                rep("DSSVA_CLEAN_f2f3_Previous_medical_condition.dta", ncol(data14)),
                rep("DSSVA_CLEAN_Treatment_Operation.dta", ncol(data15)),
                rep("DSSVA_CLEAN_Treatment_Type.dta", ncol(data16)),
                rep("DSSVA_NEW_CLEAN_f1_symptoms_lastillness.dta", ncol(data17)),
                rep("DSSVA_NEW_CLEAN_f1_symptoms_lastillness1.dta", ncol(data18)),
                rep("DSSVA_NEW_CLEAN_f2_symptoms_lastillness1.dta", ncol(data19)),
                rep("DSSVA_NEW_CLEAN_f2_symptoms_lastillness2.dta", ncol(data20)),
                rep("DSSVA_NEW_CLEAN_f2f3_Previous_medical_condition.dta", ncol(data21)),
                rep("DSSVA_NEW_CLEAN_f2f3_Previous_medical_condition1.dta", ncol(data22)),
                rep("DSSVA_NEW_CLEAN_Treatment_Type.dta", ncol(data23)),
                rep("VA_legacy.dta", ncol(data24)),
                rep("WHO_2007FM1.dta", ncol(data25)),
                rep("WHO_2007FM2.dta", ncol(data26)),
                rep("WHO2010_FORM1.dta", ncol(data27)),
                rep("WHO2010_FORM2.dta", ncol(data28)),
                rep("WHO2012_FORM1.dta", ncol(data29)),
                rep("WHO2012_FORM2.dta", ncol(data30)))

all.names <- c(names(data0), names(data1),
               names(data2), names(data3),
               names(data4), names(data5),
               names(data6), names(data7),
               names(data8), names(data9),
               names(data10), names(data11),
               names(data12), names(data13),
               names(data14), names(data15),
               names(data16), names(data17),
               names(data18), names(data19),
               names(data20), names(data21),
               names(data22), names(data23),
               names(data24), names(data25),
               names(data26), names(data27),
               names(data28), names(data29),
               names(data30))


all.files <- cbind(file.names, all.names)

write.csv(all.files, "C:/Users/HPowell/Documents/Kenya VA forms/Kenya_VA_names and files.csv")

### Trying to put the data together


new.data <- merge(data0, data1, by = c("filenum"))

head(cbind(new.data$birth_date.x, new.data$birth_date.y, new.data$vedob))
head(cbind(new.data$age_years.x, new.data$age_years.y, new.data$veyears))
head(cbind(new.data$edate, new.data$edate2.x, new.data$edate2.y))

new.data$vedob2 <- as.Date(new.data$vedob, "%Y-%m-%d")
new.data$vedod2 <- as.Date(new.data$vedod, "%Y-%m-%d")

new.data2 <- merge(data0, data2, by = "filenum")

head(cbind(new.data2$gender.x, new.data2$gender.y))
head(cbind(new.data2$birth_date.x, new.data2$birth_date.y))
head(cbind(new.data2$age_years.x, new.data2$age_years.y))
head(cbind(new.data2$edate, new.data2$edate2.x, new.data2$edate2.y))

new.data3 <- merge(data0, data3, by = "filenum")

head(cbind(new.data3$gender.x, new.data3$gender.y))
head(cbind(new.data3$birth_date.x, new.data3$birth_date.y))
head(cbind(new.data3$age_years.x, new.data3$age_years.y))
head(cbind(new.data3$edate, new.data3$edate2.x, new.data3$edate2.y))

new.data4 <- merge(data0, data4, by = "filenum")

head(cbind(new.data4$gender.x, new.data4$gender.y))
head(cbind(new.data4$birth_date.x, new.data4$birth_date.y))
head(cbind(new.data4$age_years.x, new.data4$age_years.y))
head(cbind(new.data4$edate, new.data4$edate2.x, new.data4$edate2.y))



cbind(data5$vecarhpn, data5$vecarhpn1_, data5$vecarhpn12, data5$vecarhpn1)


cbind(data6$dccod, data6$dccod1_1, data6$dccod1_2, data6$dccod1_3,
      data6$dccod1)

new.data7 <- merge(data0, data7, by = "filenum")



