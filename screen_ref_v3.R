# Always required:
#   1.	Reference GIS data: Output from the Reference Processor 
# 
# If we want to screen for habitat:
#   2.	W1_HALL: PHAB habitat metrics output from SWAMP data warehouse
# 
# If we want to include conductivity
# 3.	Observed conductivity data, found in one of these:
#  a.	Water chemistry data; or
#  b.	PHAB field data; or
#  c.	PHAB field metrics
# 4.	Predicted background conductivity:
#  a.	Output from the Indices Processor; or
#  b.	Predicted conductivity from the model/ASCI output


library(tidyverse)
library(ASCI)


screen_ref<-
  function(refGIS=refGIS, nlcd_year=2016, phab=NULL, natGIS=NULL, cond=NULL) {
    #Create a function to get max of a value, ignoring NAs, but returning NA if all values are NA (vs. the usual default of -Inf)
    safe.max = function(invector) {
      na.pct = sum(is.na(invector))/length(invector)
      if (na.pct == 1) {
        return(NA) }
      else {
        return(max(invector,na.rm=TRUE))
      }
    }
    safe.min = function(invector) {
      na.pct = sum(is.na(invector))/length(invector)
      if (na.pct == 1) {
        return(NA) }
      else {
        return(min(invector,na.rm=TRUE))
      }
    }
    
    #Assemble observed cond
    if(is.null(cond))
      cond_obs<-refGIS %>%
        select(StationCode) %>%
        unique() %>%
        mutate(MaxCond=NA,
               MinCond=NA)
    else
    {
      #Cond points to lab/field results
      if("AnalyteName" %in% names(cond))
      {
        cond_obs<-refGIS %>%
          select(StationCode) %>%
          unique() %>%
          left_join(cond) %>%
          filter(AnalyteName=="SpecificConductivity") %>%
          group_by(StationCode) %>%
          summarise(MaxCond = safe.max(Result),
                   MinCond = safe.min(Result)) %>%
          ungroup()
      }
      else #cond points to phab metrics
      {
        cond_obs<-refGIS %>%
          select(StationCode) %>%
          unique() %>%
          left_join(cond) %>%
          filter(Code=="XWSC") %>%
          group_by(StationCode) %>%
          summarise(MaxCond = safe.max(Result),
                    MinCond = safe.min(Result)) %>%
          ungroup()
      }
    }
    
    #Predict background conductivity
    ##Load qRF model; requires ASCI package
    data("rfmods")
    cond.qrf<-rfmods$cond.qrf
    ##Check for natGIS inputs
    if(is.null(natGIS))
    {
      cond_pred<-refGIS %>%
        select(StationCode) %>%
        unique() %>%
        mutate(CondQR01=NA,
               CondQR50=NA,
               CondQR99=NA,
               CondQR99c=NA)
    }
    else
    {
      cond_pred<-natGIS %>%
        transmute(StationCode=StationCode,
                  CondQR01 = predict(cond.qrf, what=c(.01), newdata=natGIS),
                  # CondQR50 = predict(cond.qrf, what=c(.5), newdata=natGIS),
                  CondQR99 = predict(cond.qrf, what=c(.99), newdata=natGIS),
                  CondQR99c=case_when(CondQR99>1000~2000,T~CondQR99))
    }
    cond_test <- cond_pred %>%
      left_join(cond_obs) %>%
      transmute(StationCode=StationCode,
                CondQR99=CondQR99,
                SpCond_pass = (MaxCond<=CondQR99c & MinCond>=CondQR01))

    
    if(is.null(phab))
    {
      phab<-refGIS %>%
        select(StationCode) %>%
        unique() %>%
        mutate(SampleDate="1/1/1950", 
               Code="W1_HALL_SWAMP", 
               Result=NA)
    }
    
    refGIS2<-refGIS %>%
      # refdf %>%
      mutate(NRST_DAM2 = ifelse(NRST_DAM<0,Inf, NRST_DAM) ) %>%
      # mutate(NRST_DAM2 = case_when(NRST_DAM<0~Inf, T~NRST_DAM) ) %>%
      transmute(StationCode=StationCode,
                CNL_PI_PCT_pass = CNL_PI_PCT < 10,
                MINES_pass = MINES==0,
                NRST_DAM_pass = NRST_DAM2 > 10,
                
                AG_WS_01_pass = AG_WS_01 < 3,
                UR_WS_01_pass = UR_WS_01 < 3,
                AGUR_WS_01_pass = AGUR_WS_01 < 5,
                CD21_WS_01_pass= CD21_WS_01  < 10,
                AG_WS_06_pass = AG_WS_06 < 3,
                UR_WS_06_pass = UR_WS_06 < 3,
                AGUR_WS_06_pass = AGUR_WS_06 < 5,
                CD21_WS_06_pass= CD21_WS_06  < 10,
                AG_WS_16_pass = AG_WS_16 < 3,
                UR_WS_16_pass = UR_WS_16 < 3,
                AGUR_WS_16_pass = AGUR_WS_16 < 5,
                CD21_WS_16_pass= CD21_WS_16  < 10,
                
                AG_1k_01_pass = AG_1k_01 < 3,
                UR_1k_01_pass = UR_1k_01 < 3,
                AGUR_1k_01_pass = AGUR_1k_01 < 5,
                CD21_1k_01_pass= CD21_1k_01  < 7,
                AG_1k_06_pass = AG_1k_06 < 3,
                UR_1k_06_pass = UR_1k_06 < 3,
                AGUR_1k_06_pass = AGUR_1k_06 < 5,
                CD21_1k_06_pass= CD21_1k_06  < 7,
                AG_1k_16_pass = AG_1k_16 < 3,
                UR_1k_16_pass = UR_1k_16 < 3,
                AGUR_1k_16_pass = AGUR_1k_16 < 5,
                CD21_1k_16_pass= CD21_1k_16  < 7,
                
                AG_5k_01_pass = AG_5k_01 < 3,
                UR_5k_01_pass = UR_5k_01 < 3,
                AGUR_5k_01_pass = AGUR_5k_01 < 5,
                CD21_5k_01_pass= CD21_5k_01  < 7,
                AG_5k_06_pass = AG_5k_06 < 3,
                UR_5k_06_pass = UR_5k_06 < 3,
                AGUR_5k_06_pass = AGUR_5k_06 < 5,
                CD21_5k_06_pass= CD21_5k_06  < 7,
                AG_5k_16_pass = AG_5k_16 < 3,
                UR_5k_16_pass = UR_5k_16 < 3,
                AGUR_5k_16_pass = AGUR_5k_16 < 5,
                CD21_5k_16_pass= CD21_5k_16  < 7,
                
                PVD_INT_WS_pass = PVD_INT_WS < 50,
                PVD_INT_5k_pass = PVD_INT_5k < 10,
                PVD_INT_1k_pass = PVD_INT_1k < 5,
                
                RDRRDEN_WS_pass = RDRRDEN_WS < 2,
                RDRRDEN_5k_pass = RDRRDEN_5k < 2,
                RDRRDEN_1k_pass = RDRRDEN_1k < 2 )
    myvars<-c("CNL_PI_PCT_pass","MINES_pass",
              # "NRST_DAM_pass",
              "PVD_INT_WS_pass","PVD_INT_5k_pass","PVD_INT_1k_pass", 
              "RDRRDEN_WS_pass","RDRRDEN_5k_pass","RDRRDEN_1k_pass")
    
    if(nlcd_year==2001)
    {
      myvars2<-c(myvars, "AG_WS_01_pass","UR_WS_01_pass", "UR_WS_01_pass", "CD21_WS_01_pass",
                 "AG_5k_01_pass","UR_5k_01_pass", "UR_5k_01_pass", "CD21_5k_01_pass",
                 "AG_1k_01_pass","UR_1k_01_pass", "UR_1k_01_pass", "CD21_1k_01_pass")
    } else
      if(nlcd_year==2006)
      {
        myvars2<-c(myvars, "AG_WS_06_pass","UR_WS_06_pass", "UR_WS_06_pass", "CD21_WS_06_pass",
                   "AG_5k_06_pass","UR_5k_06_pass", "UR_5k_06_pass", "CD21_5k_06_pass",
                   "AG_1k_06_pass","UR_1k_06_pass", "UR_1k_06_pass", "CD21_1k_06_pass")
      } else
        if(nlcd_year==2011)
        {
          myvars2<-c(myvars, "AG_WS_11_pass","UR_WS_11_pass", "UR_WS_11_pass", "CD21_WS_11_pass",
                     "AG_5k_11_pass","UR_5k_11_pass", "UR_5k_11_pass", "CD21_5k_11_pass",
                     "AG_1k_11_pass","UR_1k_11_pass", "UR_1k_11_pass", "CD21_1k_11_pass")
        } else
          if(nlcd_year==2016)
          {
            myvars2<-c(myvars, "AG_WS_16_pass","UR_WS_16_pass", "UR_WS_16_pass", "CD21_WS_16_pass",
                       "AG_5k_16_pass","UR_5k_16_pass", "UR_5k_16_pass", "CD21_5k_16_pass",
                       "AG_1k_16_pass","UR_1k_16_pass", "UR_1k_16_pass", "CD21_1k_16_pass")
          }
    
    refGIS2 %>% 
      select(StationCode, all_of(myvars2)) 
    refGIS3 <-refGIS2 %>%
      pivot_longer(cols=all_of(myvars2), names_to="metric", values_to = "screen") %>%
      group_by(StationCode) %>%
      summarise(Ref_refGIS_Ode2016 = all(screen),
                Ref_refGISfailed_Ode2016 = sum(!screen)) %>%
      ungroup()
    refGIS4<-inner_join(refGIS2,refGIS3)
    PHABmets<-refGIS %>%
      select(StationCode) %>%
      inner_join(phab %>%
                   select(StationCode, SampleDate, Code, Result) %>% 
                   filter(Code=="W1_HALL_SWAMP")
      ) %>%
      group_by(StationCode) %>%
      summarise(maxW1 =safe.max(Result)) %>%
      transmute(StationCode=StationCode,
                W1_HALL_SWAMP_pass=maxW1<1.5)
    
    refGIS5<-left_join(refGIS4, PHABmets) %>%
      left_join(cond_test) %>%
      mutate(Ref_refGIS_Field_Ode2016= case_when((W1_HALL_SWAMP_pass & Ref_refGIS_Ode2016) ~ "Reference",
                                                 !Ref_GIS_Ode2016~"Non-reference",
                                                 Ref_GIS_Ode2016 & is.na(W1_HALL_SWAMP_pass) ~ "Tentative reference",
                                                 !W1_HALL_SWAMP_pass ~ "Non-reference",
                                                 T~"xxxxx"))
    refGIS5
  }

refdf<-read.csv("RefScreening/refGIS.csv", stringsAsFactors = F)
phabdf<-read.csv("RefScreening/ReportingMetrics.csv", stringsAsFactors = F)
fielddf<-read.csv("RefScreening/phab_fieldresults.csv", stringsAsFactors = F)
labdf<-read.csv("RefScreening/Report_LabResult.csv", stringsAsFactors = F)
natdf<-read.csv("RefScreening/Indices_Metrics_Consolidated.csv", stringsAsFactors = F)
  
screen_ref(refGIS=refdf) 
screen_ref(refGIS=refdf, phab=phabdf)
screen_ref(refGIS=refdf, cond = labdf) 
screen_ref(refGIS=refdf, cond = labdf, natGIS = natdf) 
screen_ref(refGIS=refdf, cond = phabdf, natGIS = natdf) 
screen_ref(refGIS=refdf, phab= phabdf, cond = phabdf, natGIS = natdf) 





