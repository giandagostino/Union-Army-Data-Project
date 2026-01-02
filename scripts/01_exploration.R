# PURPOSE
# This is a temporary exploration script used during data cleaning.
# Not intended for reproducible output




mydata = readxl::read_xlsx('cen_union_army_whites_excel.xlsx')
library(dplyr)


name1 = mydata$recnam_0
name2 = mydata$recnam_1
name3 = mydata$recnam_5
name4 = mydata$recnam_6

print(filter(mydata[1,])


mil = read.csv("mil_union_army.csv")
dis = read.csv('dis_union_army.csv')
msr = read.csv('msr_union_army.csv')
cen = readxl::read_xlsx('cen_union_army_whites_excel.xlsx')
cmr = read.csv("cmr_union_army.csv")

options(max.print=100)

##################### Exploration 1  #####################

# Height (MIL):
#   *ID*:       recidnum
#   Date Taken: rh_date[1-5]
#   Feet:       rh_feet[1-5]
#   Qual Code:  rh_fiqc[1-5]
#   Inches:     rh_inch[1-5]

hgt_dirty <- cbind(mil$recidnum,
              mil$rh_date1, mil$rh_date2, mil$rh_date3, mil$rh_date4, mil$rh_date5,
              mil$rh_feet1, mil$rh_feet2, mil$rh_feet3, mil$rh_feet4, mil$rh_feet5,
              mil$rh_inch1, mil$rh_inch2, mil$rh_inch3, mil$rh_inch4, mil$rh_inch5,
              mil$rh_fiqc1, mil$rh_fiqc2, mil$rh_fiqc3, mil$rh_fiqc4, mil$rh_fiqc5)

hgt_dirty = as.data.frame(hgt_dirty)

# Height at enlistment:

hgt1 = na.omit(as.numeric(hgt_dirty$V12))

hgt_5ft <- (hgt_ft == "5")

hgt_ft <- na.omit(as.numeric(hgt_dirty$V7))

table(hgt_ft)
length(as.character(hgt_ft) == '5')
length(as.character(hgt_ft) == '4')
length(hgt_ft == '1')

count(hgt_ft)

hgt_inch

##################### Age at enlistment

date_enlist <- cbind(mil$recidnum,
                    mil$lstdt1_1, mil$lstdt1_2, mil$lstdt1_3,
                    mil$lstdt2_1, mil$lstdt2_2, mil$lstdt2_3)

date_enlist1 <- date_enlist[,2]
date_enlist1 <- gsub("--", "00", date_enlist1)

as.numeric(date_enlist1)



DOB <- as.numeric(mil$rb_date1)
DOB <- replace(DOB, is.na(DOB), 0)
date_enlist1 <- 

age_enlist1 <- as.numeric()-DOB


###################### Tuberculosis



# What proportion of soldiers ever had TB?

# MIL
#
#   Cause of death:      rd_ccaus / gen_rd_cause6icd (or 10icd)


# DIS
#   Duration of problem: p_bre[1-10][1-9]
#   Frequency "       ": p_frq"         "
#     ^Up to nine for both
#   DIAGNOSIS:           p_lrs[1-10]
#   Freq:                pl_frq



cause <- mil$rd_ccaus

tb_yn <- (grepl("TUBERCULOSIS", cause, fixed = TRUE))

hgt_tb <- mil[which(tb_yn == TRUE)]


##################### Joining databases

all_ids <- cbind(
  cen$recidnum[1:25675],
  cmr$recidnum[1:25675],
  dis$recidnum[1:25675],
  mil$recidnum[1:25675],
  msr$recidnum[1:25675]
)

all_ids <- na.omit(as.numeric(all_ids))

######################## Finding variables to look into
mil$dthcs
dis$i_rat

length(substr(dis$i_rel101, 0, 500))

tb_q1 <- grepl("TUBER", dis$i_rel101)
tb_q2 <- grepl("TUBER", dis$i_rel102)
unique(grepl("TUBER", dis$i_rel101))



tub_df <- dis[which(tb_q1 == T | tb_q2 == T),]

mil$ilwdsc01
na.omit(mil$afdesc01)

mil$lst
mil$mildf
mil$mildtdt
mil$mildtq
mil$othrdt01
dis$a_clst01
dis$a_clcnt
dis$a_dis02
dis$a_mlsta01
mil$afdesc

unique(mil$a_clm01[which(grepl("TUBERCULOSIS", mil$a_clm01)==T)])
length(mil$a_clm03[which(grepl("TUBER", mil$a_clm03)==T)])

tb_q <- function(X){
  data = unique(X[which(grepl("TUBER", X)==T)])
  n = length(X[which(grepl("TUBER", X)==T)])
  return(c(n, data))
}


tb_q(mil$a_clm01)

unique(mil$a_clst01[which(grepl("TUBER", mil$a_cl01)==T)])

print()

library(dplyr)


my 
for(k in 1:25675){
  
}



dis$gen_a_ococd1
dis$a_ocpat3
dis$a_ocdsc1
mil$gen_birthplace_icpsr1
clm<-unique(cbind(mil$i_bclm01, mil$i_clm01))
tb_q(dis$i_fev1)
dis$i_rat13[which(grepl("TUBERCULOSIS", dis$i_fev1)==T)]
unique(dis$i_rat11)
mil$info2t01

mil$rb_date
mil$recidnum[which(max(mil$rd_date)==19620129)]

mil[mil$recidnum==100501001,]

old <- mil %>%
  filter(recidnum==100501001)

old1<-na.omit(old)

mil$gen_birthplace_icpsr1

dis$a_mlsta
mil$gen_milocn
mil$chgstat
cen$incnam
mil$infoqc
mil$lsdys
mil$gen_lscnt_icpsr
cen$recnam
unique(mil$ro_fdt09)
mil$rw_iqc
dis$x_ext
dis$a_mlcmp1

unique(mil$dponet16)
dis$gen_a_ococd1
unique(dis$gen_a_occod1)

tb2<- full_data2[which(grepl("TUBER", full_data2$rd_cause)==T),]

unique(full_data2$recnam_6[which(grepl("TUBER", full_data2$recnam_6)==T)])


tb$gen_rd_cause_6icd
typeof(tb$gen_rd_cause_6icd)


### new vbls:
# a_apptyp, a_mlrnk, a_mlsta, a_mldsc, a_mlcmp, 
# a_mlrgt, a_rspsit, a_rspstn, 


is.na(mil$ageinj01)
mil$appage
unique(mil$bdyloc01[which(grepl("MOUTH", mil$bdyloc01)==T)])
mil$chgcomp1

unique(dis$cd_whn1)
tb_q(mil$rd_cause)
unique(mil$confnpl1)

unique(dis$d_nst11)

unique(mil$dsrtac01)
unique(mil$dschrn01)
dis$examnum


dis$g_rel

length(unique(full_data1$recidnum))
unique(mil$i_bclm01)

cen$incnam_1
unique(mil$powrslt1)


tb_q(mil$powrslt1)

dis$r_rel


length(unique(full_data2$recidnum))

tb$recnam_0[which(tb$recidnum == 2503004057)]
unique(cen$recnav_1
       
       
##########################  tb_v1  ##################################
       
############# Delinneating birth year
   

tb = fread('tb_v1.csv')


birth = as.data.frame(cbind(tb$rb_date1, tb$rb_date2, tb$rb_dtqc1, tb$rb_dtqc2, tb$recbyr_0, tb$a_xmdate, tb$a_xmdtqc, tb$a_age, tb$a_ageqc))

unique(na.omit(birth[,3]))

birth$birth_year <-
  ifelse(                        #if census, census
  !is.na(birth[,5]) & birth[,5] !="",
  birth[,5],
    ifelse(                      #if code B, use rb_date1
    birth[,3] == 'B',
    birth[,1],
      ifelse(                    #if code B, use rb_date2
      birth[,4] == 'B',
      birth[,2],
                                 #** ADD OTHER CENSUS AGE
      
        ifelse(                  #age at last exam
        !is.na(birth[,6]) & birth[6] !="",
        (as.numeric(substr(birth[,6], 1, 4))-as.numeric(birth[,8])),
        birth[,1]                 #if census n/a, use rb_date1
        
  ))))

sum(birth[,6] != "")


length(birth$V9[which(!is.na(birth$bearth))])
length(birth$V4[which(birth$V4==5)])
length(birth$V4[which(birth$V3==5)])

length(birth$birth_year[which(!is.na(birth$birth_year) & birth$birth_year != "")])

cen$recage_5


length(cen$recbyr_0[which(!is.na(cen$recage_6) & cen$recage_6 != "")])

length(cen$recbyr_0[which(!is.na(cen$recbyr_0) & birth$recage_6 != "")])


############# Pie-chart of birth state

table(tb$a_brthst)

brst <- tb$a_brthst

non_na <- function(x){            # General function for determining non-na values
  list = x[!is.na(x) & x != ""]
  len = length(list)
  prop = len/length(x)
  return(list(len, prop, list))
}


non_na(tb$a_brthst)
freq = as.data.frame(table(brst[!is.na(brst) & brst != ""]))

pie(table(brst[!is.na(brst) & brst != ""]))

ggplot(freq, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  labs(fill = 'Category') +
  ggtitle("Pie Chart with ggplot2")

head(freq)


##########################  tb_v2  ##################################

############# Delinneating birth year

tb_birth <- all %>%
  select(
    rb_date1, rb_date2, rb_dtqc1, rb_dtqc2,
    recbyr_0, a_xmdate, a_xmdtqc, a_age, a_ageqc,
    lstag1_1, lstag1_2, lstag1_3,
    lstag2_1, lstag2_2, lstag2_3,
    lstdt1_1, lstdt1_2, lstdt1_3,
    lstdt2_1, lstdt2_2, lstdt2_3,
    lsdqc1_1, lsdqc1_2, lsdqc1_3,
    lsdqc2_1, lsdqc2_2, lsdqc2_3,
    recage_5, recage_6, recabd_0, recabd_1
  ) %>%
as.data.frame()

ntb_birth <- ntb %>%
  select(
    rb_date1, rb_date2, rb_dtqc1, rb_dtqc2,
    recbyr_0, a_xmdate, a_xmdtqc, a_age, a_ageqc,
    lstag1_1, lstag1_2, lstag1_3,
    lstag2_1, lstag2_2, lstag2_3,
    lstdt1_1, lstdt1_2, lstdt1_3,
    lstdt2_1, lstdt2_2, lstdt2_3,
    lsdqc1_1, lsdqc1_2, lsdqc1_3,
    lsdqc2_1, lsdqc2_2, lsdqc2_3,
    recage_5, recage_6, recabd_0, recabd_1
  ) %>%
as.data.frame()



##########  Final Birth Year  ##########

tb_birth$YEAR <-
  ifelse(                                                                 #if census, census
    !is.na(tb_birth[,5]) & tb_birth[,5] !="" & tb_birth[,5] <2000,
    tb_birth[,5],
    ifelse(                     
      !is.na(tb_birth[,30]) & tb_birth[,30] !="" & tb_birth[,30] <110,
      (1900 - as.numeric(tb_birth[,30])),
      ifelse(
        !is.na(tb_birth[,31]) & tb_birth[,31] !="" & tb_birth[,31] <110,
        (1910 - as.numeric(tb_birth[,31])),
        ifelse(                                                       
          !is.na(tb_birth[,29]) & tb_birth[,29] !="" & tb_birth[,29] <110,
          (1860 - as.numeric(tb_birth[,29])),
          ifelse(  
            !is.na(tb_birth[,28]) & tb_birth[,28] !="" & tb_birth[,28] <110,
            (1850 - as.numeric(tb_birth[,28])),
            ifelse(                                                
              !is.na(tb_birth[,6]) & tb_birth[6] !="",                      
              (as.numeric(substr(tb_birth[,6], 1, 4))-as.numeric(tb_birth[,8])),
              ifelse(
                  tb_birth[,3] == 'B',
                  tb_birth[,1],
                  ifelse(                                    #if code B, use rb_date2
                    tb_birth[,4] == 'B',
                    tb_birth[,2],
                    tb_birth[,1]                             #if census n/a, use rb_date1
                      
))))))))



x1_1 <- as.numeric(substr(tb_birth$lstdt1_1, 1, 4)) - tb_birth$lstag1_1
x1_2 <- as.numeric(substr(tb_birth$lstdt1_2, 1, 4)) - tb_birth$lstag1_2
x1_3 <- as.numeric(substr(tb_birth$lstdt1_3, 1, 4)) - tb_birth$lstag1_3
x2_1 <- as.numeric(substr(tb_birth$lstdt2_1, 1, 4)) - tb_birth$lstag2_1
x2_2 <- as.numeric(substr(tb_birth$lstdt2_2, 1, 4)) - tb_birth$lstag2_2
x2_3 <- as.numeric(substr(tb_birth$lstdt2_3, 1, 4)) - tb_birth$lstag2_3

tb_lst <- cbind(x1_1, x1_2, x1_3, x2_1, x2_2, x2_3)
tb_lst <- as.data.frame(tb_lst)

lst_tb_birth <- tb_birth %>%
  filter(YEAR == "" | is.na(YEAR)) %>%
  select(lstag1_1:lsdqc2_3)

avg_tb_lst <- tb_lst %>%
  mutate(across(where(is.character), as.numeric)) %>% 
  mutate(row_mean = rowMeans(across(where(is.numeric)), na.rm = TRUE))

non_na(tb_birth$YEAR)

tb_birth$YEAR <-
ifelse(
  tb_birth[,27] == 'B',
  (as.numeric(substr(tb_birth[,21], 1, 4))-as.numeric(tb_birth[,15])),
  ifelse(
    tb_birth[,26] == 'B',
    (as.numeric(substr(tb_birth[,20], 1, 4))-as.numeric(tb_birth[,14])),
    ifelse(
      tb_birth[,25] == 'B',
      (as.numeric(substr(tb_birth[,19], 1, 4))-as.numeric(tb_birth[,13])),
      ifelse(
        tb_birth[,24] == 'B',
        (as.numeric(substr(tb_birth[,18], 1, 4))-as.numeric(tb_birth[,12])),
        ifelse(
          tb_birth[,23] == 'B',
          (as.numeric(substr(tb_birth[,17], 1, 4))-as.numeric(tb_birth[,11])),
          ifelse(
            tb_birth[,22] == 'B',
            (as.numeric(substr(tb_birth[,16], 1, 4))-as.numeric(tb_birth[,10])),
            ifelse(
              !is.na(avg_tb_lst[,7]) & avg_tb_lst[,7] !="",
              as.numeric(avg_tb_lst[,7]),
              tb_birth[,1])
))))))


tb_birth$YEAR_cd <-
  ifelse(                                                                 #if census, census
    !is.na(tb_birth[,5]) & tb_birth[,5] !="" & tb_birth[,5] <2000,
    '0',
    ifelse(                     
      !is.na(tb_birth[,30]) & tb_birth[,30] !="" & tb_birth[,30] <110,
      '0',
      ifelse(
        !is.na(tb_birth[,31]) & tb_birth[,31] !="" & tb_birth[,31] <110,
        '0a',
        ifelse(                                                           #if code B, use rb_date1
          !is.na(tb_birth[,29]) & tb_birth[,29] !="" & tb_birth[,29] <110,
          '0b',
          ifelse(  
            !is.na(tb_birth[,28]) & tb_birth[,28] !="" & tb_birth[,28] <110,
            '0c',
            ifelse(                                                      #age at last exam
              !is.na(tb_birth[,6]) & tb_birth[6] !="",                      
              '1',
              ifelse(
                tb_birth[,3] == 'B',
                '2a',
                ifelse(                                                      #if code B, use rb_date2
                  tb_birth[,4] == 'B',
                  '2b',
                  ifelse(
                    tb_birth[,27] == 'B',
                    '3a',
                    ifelse(
                      tb_birth[,26] == 'B',
                      '3b',
                      ifelse(
                        tb_birth[,25] == 'B',
                        '3c',
                        ifelse(
                          tb_birth[,24] == 'B',
                          '3d',
                          ifelse(
                            tb_birth[,23] == 'B',
                            '3e',
                            ifelse(
                              tb_birth[,22] == 'B',
                              '3f',
                              ifelse(
                                !is.na(avg_tb_lst[,7]) & avg_tb_lst[,7] !="",
                                '4',
                                '5'                                       #if census n/a, use rb_date1
              
    )))))))))))))))

yr <- cbind(tb_birth$YEAR, tb_birth$YEAR_cd)
write.csv(yr, "tb_byr_clean.csv")

ntb_YEAR_cl <- as.numeric(substr(ntb_birth$YEAR, 1, 4))
ntb_YEAR_cl <- YEAR_cl[which(YEAR_cl>1700 & YEAR_cl<1865)]

tb_YEAR_cl <- as.numeric(substr(tb_birth$YEAR, 1, 4))
tb_YEAR_cl <- tb_YEAR_cl[which(tb_YEAR_cl>1700 & tb_YEAR_cl<1865 & !is.na(tb_YEAR_cl))]

hist(ntb_YEAR_cl, breaks=50)
hist(ntb_tb_YEAR_cl, breaks=50)
mean(YEAR_cl[which(!is.na(YEAR_cl))])
summary(YEAR_cl)



############################# DEATH YEAR #######################################

non_na(tb$rd_date)
non_na(tb$dth)

tb_death <- tb %>%
  select(
    rd_date, rd_dtqc, dthdt01, dthdt02, dthdt03, dthdt04, dthdt05, dthdt06,
    dthdt07, dthdt08, dthdt09, dthdt10, dthdt11, dthdt12, dthdt13, dthdt14,
    dthdt15, dthdt16, dthdt17, dthdt18, dthdt19, dthdt20
  ) %>%
as.data.frame()

ntb_death <- ntb %>%
  select(
    rd_date, rd_dtqc, dthdt01, dthdt02, dthdt03, dthdt04, dthdt05, dthdt06,
    dthdt07, dthdt08, dthdt09, dthdt10, dthdt11, dthdt12, dthdt13, dthdt14,
    dthdt15, dthdt16, dthdt17, dthdt18, dthdt19, dthdt20
  ) %>%
as.data.frame()


DTH_YR <- ifelse(                        #if census, census
  !is.na(tb_death[,1]) & tb_death[,1] !="",
  tb_death[,1],
  ifelse(                      #if code B, use rb_date1
    ntb_birth[,3] == 'B',
    ntb_birth[,1],
    ifelse(                    #if code B, use rb_date2
      ntb_birth[,4] == 'B',
      ntb_birth[,2],
      #** ADD OTHER CENSUS AGE
      
      ifelse(                  #age at last exam
        !is.na(ntb_birth[,6]) & ntb_birth[6] !="",
        (as.numeric(substr(ntb_birth[,6], 1, 4))-as.numeric(ntb_birth[,8])),
        ntb_birth[,1]                 #if census n/a, use rb_date1
        
      ))))
  


ntb_death_na <- ntb_death %>%
  filter(is.na(rd_date) | rd_date == "")

non_empty_values <- ntb_death_na %>%
  unlist() %>%
  discard(~ is.na(.) || . == "")


tb_DTH_cl <- as.numeric(substr(tb$rd_date, 1, 4))
tb_DTH_cl <- tb_DTH_cl[which(tb_DTH_cl>1800 & tb_DTH_cl<2000)]
tb_dth_age <- tb_DTH_cl - tb_YEAR_cl

ntb_YEAR_cl <- as.numeric(substr(ntb_birth$YEAR, 1, 4))
ntb_DTH_cl <- as.numeric(substr(ntb$rd_date, 1, 4))
ntb_dth_age <- ntb_DTH_cl - ntb_YEAR_cl

hist(tb_dth_age[which(tb_dth_age >0 & tb_dth_age < 110)], breaks=50)
hist(ntb_dth_age[which(ntb_dth_age >0 & ntb_dth_age < 110)], breaks=50)
  
hist(tb_DTH_cl[which(tb_DTH_cl > 1800 & tb_DTH_cl < 2000)], breaks=50, main="TB")
hist(ntb_DTH_cl[which(ntb_DTH_cl > 1800 & ntb_DTH_cl < 2000)], breaks=50, main='non_TB')

non_na(ntb_dth_age) #19242
non_na(tb_dth_age) #1575

ntb = fread("ntb_v4.csv")
tb = fread("tb_v5.csv")

write.csv(ntb, 'ntb_v4.csv')
write.csv(tb, 'tb_v5.csv')


#############################  ENLISTMENT INFO  ####################################

as.data.frame(ntb)
as.data.frame(tb)

all <- rbind(tb,ntb)

lst <- all %>%
  select(lstag1_1, lstag1_2, lstag1_3,
         lstag2_1, lstag2_2, lstag2_3,
         lstdt1_1, lstdt1_2, lstdt1_3,
         lstdt2_1, lstdt2_2, lstdt2_3,
         lsdqc1_1, lsdqc1_2, lsdqc1_3,
         lsdqc2_1, lsdqc2_2, lsdqc2_3,
         dchdt1_1, dchdt1_2, dchdt1_3,
         dchdt2_1, dchdt2_2, dchdt2_3,
         dchqc1_1, dchqc1_2, dchqc1_3,
         dchqc2_1, dchqc2_2, dchqc2_3,
         dchrn1_1, dchrn1_2, dchrn1_3,
         dchrn2_1, dchrn2_2, dchrn2_3, 
         dschrn01, dschrn02, dschrn03, dschrn04, dschrn05,
         dschrn06, dschrn07, dschrn08, dschrn09, dschrn10,
         dschrn11, dschrn12, dschrn13, dschrn14, dschrn15,
         dschrn16, dschrn17, dschrn18, dschrn19, dschrn20,
         dschdt01, dschdt02, dschdt03, dschdt04, dschdt05,
         dschdt06, dschdt07, dschdt08, dschdt09, dschdt10,
         dschdt11, dschdt12, dschdt13, dschdt14, dschdt15,
         dschdt16, dschdt17, dschdt18, dschdt19, dschdt20,
         dschqc01, dschqc02, dschqc03, dschqc04, dschqc05,
         dschqc06, dschqc07, dschqc08, dschqc09, dschqc10,
         dschqc11, dschqc12, dschqc13, dschqc14, dschqc15,
         dschqc16, # dschqc17, dschqc18, dschqc19,
         dschqc20,
  )


#######  First enlistment


lstdt <- all %>%
  select(
    lstdt1_1, lstdt1_2, lstdt1_3,
    lstdt2_1, lstdt2_2, lstdt2_3
  )

lstdt <- lstdt %>%
  mutate(across(everything(),
                ~ as.numeric(substr(as.character(.x), 1, 4))))

lstdt <- lstdt %>%
  mutate(
    min_value = do.call(pmin, c(across(where(is.numeric)), na.rm = TRUE))
  )


lst_min <- ifelse(
  sapply(lstdt, function(col) any(grepl("^\\d{8}$", col))) == F,
  apply(lstdt, 1, function(row) min(as.numeric(row), na.rm = T)),
  NA
)
  
  

lstdt$lst_min <- lst_min

lstdt$lst_min <- ifelse(sapply(lstdt, function(col) any(grepl("^\\d{8}$", col))) == T,
                        lst_min,
                        NA)


sum(grepl("^\\d{8}$", lstdt$lst_min))/39338



####################### Discharge / Desertion #################################

# Reasons for end of service:
#   Final discharge
#      Max(dschdt/dchdt)
#   Desertion
#      Deserted from post
#           Date they were noticed missing
#   Death
#           Date of death
#      Death due to illness/wound (non-TB)
#      Death due to TB
#      TB-affected ind w non-TB death
#
# Questions
#    
#   Is desertion, death, and discharge all under dschdt/dchdt??
#
#   Take everything from recruit under death and desertion categories (rsn),
#   show discharge dates from those two variables.

end <- all %>%
  select(
    recidnum,
    dchdt1_1, dchdt1_2, dchdt1_3,
    dchdt2_1, dchdt2_2, dchdt2_3,
    dschdt01, dschdt02, dschdt03, dschdt04, dschdt05,
    dschdt06, dschdt07, dschdt08, dschdt09, dschdt10,
    dschdt11, dschdt12, dschdt13, dschdt14, dschdt15, 
    dschdt16, dschdt17, dschdt18, dschdt19, dschdt20,
    dschrn01, dschrn02, dschrn03, dschrn04, dschrn05,
    dschrn06, dschrn07, dschrn08, dschrn09, dschrn10,
    dschrn11, dschrn12, dschrn13, dschrn14, dschrn15,
    dschrn16, dschrn17, dschrn18, dschrn19, dschrn20,
    unftc1_1, unftc1_2, unftc1_3,
    unftc2_1, unftc2_2, unftc2_3,
    unftd1_1, unftd1_2, unftd1_3,
    unftd2_1, unftd2_2, #unftd2_3,
    
  # ADD DESERTION DATE
  # ADD DEATH DATE
  )

non_na(end$dchdt1_2)
max(end$dchdt2_3)

end$recidnum[which(end$dchdt1_2== "19190728")]

all$recidnum
mil$recidnum

serve <- all %>%
  select(
    lstdt1_1, dchdt1_1,
    lstdt1_2, dchdt1_2,
    lstdt1_3, dchdt1_3,
    lstdt2_1, dchdt2_1,
    lstdt2_2, dchdt2_2,
    lstdt2_3, dchdt2_3
  )

btwn_lst <- as.numeric(serve[,3]) - as.numeric(serve[,2])
mean(abs(na.omit(btwn_lst)))


# Finding someone w/o TB that died in the war (possibly other inconsistencies w dch and dsch)

ntb_dead <- ntb |>
  select(
    recidnum,
    
#death date 
    
    dthdt01, dthdt02, dthdt03, dthdt04, dthdt05,
    dthdt06, dthdt07, dthdt08, dthdt09, dthdt10,
    dthdt11, dthdt12, dthdt13, dthdt14, dthdt15,
    dthdt16, dthdt17, dthdt18, dthdt19, dthdt20,
    
#death cause
    
    dthcs01, dthcs02, dthcs03, dthcs04, dthcs05,
    dthcs06, dthcs07, dthcs08, dthcs09, dthcs10, dthcs11,
    dthcs12, dthcs13, dthcs14, dthcs15, dthcs16, dthcs17,
    dthcs18, dthcs19, dthcs20

  )

#306, 308, 309
#2866, 2868, 2869


rec3 <- tline()




#######################  Desertion Investigation  ##############################


# Why were soldiers deserting their post?
# What is the difference between desertion and AWOL designation (milcact)
# 



rsn <- cbind(as.numeric(all$recidnum), all$milcrsn1, all$milcrsn2, all$milcrsn3, all$milcrsn4, all$milcrsn5)
unique(rsn[,2])
rsn <- as.data.frame(rsn)

rsn <- rsn %>%
  rename(recidnum = 1, rsn1 = 2, rsn2 = 3, rsn3 = 4, rsn4 = 5, rsn5 = 6) %>%
  mutate(recidnum = as.numeric(recidnum))

categorize <- function(v){
  case_when(
    grepl('awol', v, ignore.case = T) ~ "AWOL",
    grepl('capture', v, ignore.case = T) | grepl('POW', text, ignore.case = T) | grepl('prison', text, ignore.case = T) ~ "CAPTURE",
    grepl('CONDUCT', v, ignore.case = T) ~ 'CONDUCT',
    grepl('furlough', v, ignore.case = T) ~ 'FURLOUGH',
    grepl('desert', v, ignore.case = T) ~ 'DESERTION',
    grepl('died', v, ignore.case = T) | grepl('death', text, ignore.case = T) | grepl('kia', text, ignore.case = T) ~ 'DIED',
    grepl('medic', v, ignore.case = T) | grepl('sick', text, ignore.case = T) | prepl('ill', text, ignore.case = T) ~ 'MEDICAL',
    grepl('personal', v, ignore.case = T) ~ 'PERSONAL',
    grepl('', v, ignore.case = T) ~ 'NA',
    T ~ 'MISC'
  )





table(rsn$category)

rsn_lst <- all %>%
  select(recidnum,
    lstdt1_1, lstdt1_2, lstdt1_3,
    lstdt2_1, lstdt2_2, lstdt2_3,
    
    dchdt1_1, dchdt1_2, dchdt1_3,
    dchdt2_1, dchdt2_2, dchdt2_3,
    dschdt01, dschdt02, dschdt03, dschdt04, dschdt05,
    dschdt06, dschdt07, dschdt08, dschdt09, dschdt10,
    dschdt11, dschdt12, dschdt13, dschdt14, dschdt15, 
    dschdt16, dschdt17, dschdt18, dschdt19, dschdt20,
    
    milctdt1, milctdt2, milctdt3, milctdt4, milctdt5,
    dsrtdt01, dsrtdt02, dsrtdt03, dsrtdt04, dsrtdt05,
    dsrtdt06, dsrtdt07, dsrtdt08, dsrtdt09, dsrtdt10,
    dsrtdt11, dsrtdt12, dsrtdt13, dsrtdt14, dsrtdt15, 
    dsrtdt16, dsrtdt17, dsrtdt18, dsrtdt19, dsrtdt20
  )


rsn_lst <- tibble(rsn_lst)

joined <- 
  full_join(rsn_lst, rsn, by="recidnum")


awol_lst <- joined %>%
  filter(category == 'AWOL')

death_lst <- joined %>%
  filter(category == 'DIED')

dsrt_lst <- joined %>%
  filter(category == 'DESERTION')

awol_lst <- joined %>%
  filter(category == 'AWOL')


# did these people ever return to duty after AWOL or desertion


##########################  View Admit/Rels Dates  #############################


admt_rels_check <- all |>
  select(
    admtdt01, relsdt01, admtdt02, relsdt02, admtdt03, relsdt03, admtdt04, relsdt04, admtdt05, relsdt05, admtdt06, relsdt06, admtdt07, relsdt07, admtdt08, relsdt08, admtdt09, relsdt09, admtdt10, relsdt10, admtdt11, relsdt11, admtdt12, relsdt12, admtdt13, relsdt13, admtdt14, relsdt14, admtdt15, relsdt15, admtdt16, relsdt16, admtdt17, relsdt17, admtdt18, relsdt18, admtdt19, relsdt19, admtdt20, relsdt20
  )


### Can I use lst if there's no rels?
   ##### Only works if =< 6 hosp. Check:

      n = nrow(all)
      non_na_cells <- sum(!is.na(admt_rels_check[, 13:40]) & admt_rels_check[, 13:40] != "")
      n_cell = nrow(admt_rels_check)*ncol(admt_rels_check[, 13:40])
      non_na_cells/n_cell
      
      # 0.028 prop of cells filled for all
      # 0.060 prop of cells filled for tb
      # 0.026 prop of cells filled for ntb

   ##### Only works if there is lst in between admt and next admit.
   ##### If only there's no reenlistment --> need to clean all of the dates first


### What does discharge look like?


dch <- all |>
  select(
    dchdt1_1, dchdt1_2, dchdt1_3,
    dchdt2_1, dchdt2_2, dchdt2_3,
    dschdt01, dschdt02, dschdt03, dschdt04, dschdt05,
    dschdt06, dschdt07, dschdt08, dschdt09, dschdt10,
    dschdt11, dschdt12, dschdt13, dschdt14, dschdt15, 
    dschdt16, dschdt17, dschdt18, dschdt19, dschdt20
  )
      
      
   
########################   Combining lst and hosp   ############################

# *** If coming back to this, make sure to run chunk 2 and 3 from 
#   Recruit timelines ***

# This combined dataset should inform me of what to do in these situations:
#     lst w/ no rexit
#     rexit w no lst
#     lst/dch approx duplicate
#     no dch but yes rexit


rlst_hosp <- full_join(rlst,
  hosp, by = "recidnum")

r1 <- ind_timeline(rlst_hosp, 1)  # Simple example, everything lines up
r2 <- ind_timeline(rlst_hosp, 10)  # A good example of many admts (03-05)
r3 <- ind_timeline(rlst_hosp, 2000)
r4 <- ind_timeline(rlst_hosp, 8000)
r5 <- ind_timeline(rlst_hosp, 38000)
r6 <- ind_timeline(rlst_hosp, 39000)



rlst_hosp_rexit <- rlst_hosp |>
  filter(if_any(dsrtdt01:pow5, ~!is.na(.x)))


r7 <- ind_timeline(rlst_hosp_rexit, 200)


# How many times does a column type repeat sequentially when put through 
# sorting algorithm?


colnames <- colnames(rlst_hosp)

event_cols <- setdiff(colnames(rlst_hosp |> select(-n_hosp)), "recidnum")

map_event_category <- function(col) {
  case_when(
    str_detect(col, "^admtdt") ~ "hospital_admit",
    str_detect(col, "^relsdt") ~ "hospital_release",
    str_detect(col, "^lstdt")  ~ "enlistment",
    str_detect(col, "^mildfdt") ~ "enlistment",      # same bucket
    str_detect(col, "^dschdt") ~ "discharge",
    str_detect(col, "^dchdt")  ~ "discharge2",
    str_detect(col, "^dsrtdt") ~ "desertion_date",
    str_detect(col, "^dsrt")   ~ "desertion",
    str_detect(col, "^pow")    ~ "pow",
    str_detect(col, "^dth")    ~ "death",
    TRUE ~ "other"
  )
}

count_repeated <- rlst_hosp %>%
  pivot_longer(
    cols = all_of(event_cols),
    names_to = "event_type_raw",
    values_to = "event_date_raw"
  ) %>%
  mutate(
    event_date = ymd(event_date_raw, quiet = TRUE),
    event_category = map_event_category(event_type_raw)
  ) %>%
  filter(!is.na(event_date)) %>%                       # drop unparseable rows
  arrange(recidnum, event_date) %>%
  group_by(recidnum) %>%
  mutate(
    # TRUE if consecutive categories match AND they are not enlistment
    repeat_flag = event_category == lag(event_category) &
      event_category != "enlistment"
  ) %>%
  summarise(has_repeat = any(repeat_flag, na.rm = TRUE)) %>%
  summarise(n_with_repeated_categories = sum(has_repeat))

15607/39338

repeat_type_counts <- rlst_hosp |>
  pivot_longer(
    cols = all_of(event_cols),
    names_to = "event_type_raw",
    values_to = "event_date_raw"
  ) |>
  mutate(
    event_date = ymd(event_date_raw, quiet = TRUE),
    event_category = map_event_category(event_type_raw)
  ) |>
  filter(!is.na(event_date)) |>        # drop unparseable rows
  arrange(recidnum, event_date) |>
  group_by(recidnum) |>
  mutate(
    is_repeat = event_category == lag(event_category) &
      event_category != "enlistment"       # enlistment repeats ignored
  ) |>
  ungroup() |>
  filter(is_repeat) |>                  # keep only repeated consecutive categories
  count(event_category, name = "repeat_count")









