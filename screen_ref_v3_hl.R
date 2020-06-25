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
    
    #What kind of PHAB metrics do you have?
    
    if(is.null(phab)) #If no phab is specified, make a dummy data frame with NAs for screening
    {
      phab<-refGIS %>%
        select(StationCode) %>%
        unique() %>%
        mutate(SampleDate="1/1/1950", 
               Code="W1_HALL_SWAMP", 
               Result=NA)
    }
    
    #Some phab data sources use "Code" and other use "Variable" as the column name. This function expects "Code" so we re-write if "Variable" is present.
    if("Variable" %in% names(phab) ) { 
      phab<-phab %>%
        rename(Code=Variable)
    }
    
    #If no cond is specified, make a dummy data frame with NAs for screening. Assume "lab" format of data
    if(is.null(cond))
      cond_obs<-refGIS %>%
        select(StationCode) %>%
        unique() %>%
        mutate(SampleDate="1/1/1950",
               AnalyteName = "SpecificConductivity",
               Replicate=1,
               Result=NA)
    else
    {
      #Cond points to lab/field results
      #HEILI: This takes the min/max. Instead, base make it evaluate each date separately.
      #RAFI: See comment inline below.
      #HEILI: Replicates aren't common, but possible. Not sure how to handle that--Maybe take max within a sample date?
      #RAFI: I've left the max and min in as defaults in case there are replicates - could also use mean? and simply summarise()
      #HEILI: Be careful about date formats! Maybe we can assume mdy?
      #RAFI: If StationCode is grouping correctly, and that column is already in character format, then SampleDate should perform similarly since it too is a character column.
      if("AnalyteName" %in% names(cond))
      {
        cond_obs<-refGIS %>%
          select(StationCode) %>%
          unique() %>%
          left_join(cond) %>%
          filter(AnalyteName=="SpecificConductivity") %>%
          group_by(StationCode, SampleDate) %>% # RAFI: First, group by station, then by date, then summarise accordingly.
          summarise(MaxCond = safe.max(Result),
                   MinCond = safe.min(Result)) %>%
          ungroup()
      }
      else #cond points to phab metrics
        #PHAB metrics sometimes include conductivity measures. It's called XWSC. 
        #This code should result in a cond_obs object with the same format as the code produced on lines 76-84
      {
        cond_obs<-refGIS %>%
          select(StationCode) %>%
          unique() %>%
          left_join(cond) %>%
          filter(Code=="XWSC") %>% 
          group_by(StationCode, SampleDate) %>%
          summarise(MaxCond = safe.max(Result),
                    MinCond = safe.min(Result)) %>%
          ungroup()
      }
    }
    
    #Predict background conductivity
    ##Load qRF model; requires ASCI package.
    data("rfmods")
    cond.qrf<-rfmods$cond.qrf
    ##Check for natGIS inputs
    #If natGIS is missing, you can't calculate background conductivity or screen based on it. 
    #Create a dummy df
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
      #If you have natGIS, calculate background conductivity
      #Calculate 1st percentile and 99th percentile using quantregForest model
      #If the high end prediction is >1000, we don't trust it, and simply use a cap of 2000.
      cond_pred<-natGIS %>%
        transmute(StationCode=StationCode,
                  CondQR01 = predict(cond.qrf, what=c(.01), newdata=natGIS),
                  # CondQR50 = predict(cond.qrf, what=c(.5), newdata=natGIS),
                  CondQR99 = predict(cond.qrf, what=c(.99), newdata=natGIS),
                  CondQR99c=case_when(CondQR99>1000~2000,T~CondQR99))
    }
    #HEILI: Replace this screen so it doesn't screen on min/max, but on sample date.
    cond_test <- cond_pred %>%
      left_join(cond_obs) %>%
      transmute(StationCode=StationCode, SampleDate=SampleDate,
                CondQR99=CondQR99,
                SpCond_pass = (MaxCond<=CondQR99c & MinCond>=CondQR01)) 

    

    #This code screens all GIS landscape data.
    refGIS2<-refGIS %>%
      mutate(NRST_DAM2 = ifelse(NRST_DAM<0,Inf, NRST_DAM) ) %>%
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
                AG_WS_11_pass = AG_WS_11 < 3,
                UR_WS_11_pass = UR_WS_11 < 3,
                AGUR_WS_11_pass = AGUR_WS_11 < 5,
                CD21_WS_11_pass= CD21_WS_11  < 10,
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
                AG_1k_11_pass = AG_1k_11 < 3,
                UR_1k_11_pass = UR_1k_11 < 3,
                AGUR_1k_11_pass = AGUR_1k_11 < 5,
                CD21_1k_11_pass= CD21_1k_11  < 7,
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
                AG_5k_11_pass = AG_5k_11 < 3,
                UR_5k_11_pass = UR_5k_11 < 3,
                AGUR_5k_11_pass = AGUR_5k_11 < 5,
                CD21_5k_11_pass= CD21_5k_11  < 7,
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
    #Here's the set of screening variables you always need.
    myvars<-c("CNL_PI_PCT_pass","MINES_pass",
              "NRST_DAM_pass",
              "PVD_INT_WS_pass","PVD_INT_5k_pass","PVD_INT_1k_pass", 
              "RDRRDEN_WS_pass","RDRRDEN_5k_pass","RDRRDEN_1k_pass")
    
    #The NLCD variables depend on the specified year (default 2016)
    #We will eventually need to update when the next set of NLCD data become available.
    if(nlcd_year==2001)
    {
      myvars2<-c(myvars, "AG_WS_01_pass","AGUR_WS_01_pass", "UR_WS_01_pass", "CD21_WS_01_pass",
                 "AG_5k_01_pass","AGUR_5k_01_pass", "UR_5k_01_pass", "CD21_5k_01_pass",
                 "AG_1k_01_pass","AGUR_1k_01_pass", "UR_1k_01_pass", "CD21_1k_01_pass")
    } else
      if(nlcd_year==2006)
      {
        myvars2<-c(myvars, "AG_WS_06_pass","AGUR_WS_06_pass", "UR_WS_06_pass", "CD21_WS_06_pass",
                   "AG_5k_06_pass","AGUR_5k_06_pass", "UR_5k_06_pass", "CD21_5k_06_pass",
                   "AG_1k_06_pass","AGUR_1k_06_pass", "UR_1k_06_pass", "CD21_1k_06_pass")
      } else
        if(nlcd_year==2011)
        {
          myvars2<-c(myvars, "AG_WS_11_pass","AGUR_WS_11_pass", "UR_WS_11_pass", "CD21_WS_11_pass",
                     "AG_5k_11_pass","AGUR_5k_11_pass", "UR_5k_11_pass", "CD21_5k_11_pass",
                     "AG_1k_11_pass","AGUR_1k_11_pass", "UR_1k_11_pass", "CD21_1k_11_pass")
        } else
          if(nlcd_year==2016)
          {
            myvars2<-c(myvars, "AG_WS_16_pass","AGUR_WS_16_pass", "UR_WS_16_pass", "CD21_WS_16_pass",
                       "AG_5k_16_pass","AGUR_5k_16_pass", "UR_5k_16_pass", "CD21_5k_16_pass",
                       "AG_1k_16_pass","AGUR_1k_16_pass", "UR_1k_16_pass", "CD21_1k_16_pass"
                       )
          }
    
    #This code aggregates across screens
    refGIS3 <-refGIS2 %>%
      select(StationCode, all_of(myvars2)) %>%
      pivot_longer(cols=all_of(myvars2), names_to="metric", values_to = "screen") %>%
      group_by(StationCode) %>%
      summarise(Ref_GIS_Ode2016 = all(screen), #This is T/F, does it pass all GIS screens?
                Ref_GISfailed_Ode2016 = sum(!screen)) %>% #This is an integer: How many GIS screens were failed?
      ungroup()
    refGIS4<-inner_join(refGIS2,refGIS3) #I add the two new variables in refGIS3 to refGIS2
    
    #Here we start screening the phab data
    #HEILI: Code on line 259 aggregates across dates. Instead we want to screen for each date You'll never have to worry about replicates for phab data.
    #RAFI: See inline comment below.
    PHABmets<-refGIS %>% #First, ensure that all sites are included.
      select(StationCode) %>%
      inner_join(phab %>%
                   select(StationCode, SampleDate, Code, Result) %>% 
                   filter(Code=="W1_HALL_SWAMP")
      ) %>%
      group_by(StationCode, SampleDate) %>%
      summarise(maxW1 =safe.max(Result)) %>% # RAFI: I've added a similar grouping by date to the previous line as I did above. #RDM: We don't really need max, but OK to leave in 
      transmute(StationCode=StationCode,
                W1_HALL_SWAMP_pass=maxW1<1.5)

    #This code puts all the pieces together
    #And it aggreates GIS screens with W1_Hall. 
    #It does not yet incorporate conductivity. Don't worry about that.
    refGIS5<-left_join(refGIS4, PHABmets) %>%
      left_join(cond_test) %>%
      mutate(Ref_GIS_Field_Ode2016= case_when((W1_HALL_SWAMP_pass & Ref_GIS_Ode2016) ~ "Reference",
                                                 !Ref_GIS_Ode2016~"Non-reference",
                                                 Ref_GIS_Ode2016 & is.na(W1_HALL_SWAMP_pass) ~ "Tentative reference",
                                                 !W1_HALL_SWAMP_pass ~ "Non-reference",
                                                 T~"xxxxx"))
    refGIS5
  }


#Here are a bunch of test data to play with

###refGIS includes GIS landscape screens. Always required
# refdf<-read.csv("RefScreening/refGIS.csv", stringsAsFactors = F)
# refdf<-read.csv("RefScreening/marco_test_061720/RefScreen_Metrics_Consolidated_Test.csv", stringsAsFactors = F)
# refdf<-read.csv("RefScreening/marco_test_061720/New folder/RefScreen_GISMetrics_PSA2017.csv", stringsAsFactors = F)

refdf <- refgis

##PHAB data includes W1_HALL, and sometimes XWSC
# phabdf<-read.csv("RefScreening/ReportingMetrics.csv", stringsAsFactors = F)
# phabdf<-read.csv("RefScreening/marco_test_061720/phab_metrics_Test.csv", stringsAsFactors = F)
# phabdf<-read.csv("RefScreening/marco_test_061720/New folder/RefScreen_PHABMetrics_PSA2017.csv", stringsAsFactors = F)

phabdf <- ReportingMetrics

##Here are sources of conductivity data
#Standard lab results for when conductivity is measured in a lab
#labdf<-read.csv("RefScreening/Report_LabResult.csv", stringsAsFactors = F)

labdf <- Report_LabResult

#Raw phab data includes direct measures of conductivity, and look like labdf
#fielddf<-read.csv("RefScreening/phab_fieldresults.csv", stringsAsFactors = F)

fielddf <- phab_fieldresults

##Here are GIS metrics needed to calculate background conductivity
# natdf<-read.csv("RefScreening/Indices_Metrics_Consolidated.csv", stringsAsFactors = F)

natdf <- Indices_Metrics_Consolidated
  
screen_ref(refGIS=refdf) 
screen_ref(refGIS=refdf, phab=phabdf)
screen_ref(refGIS=refdf, cond = labdf) 
screen_ref(refGIS=refdf, cond = labdf, natGIS = natdf) 
screen_ref(refGIS=refdf, cond = phabdf, natGIS = natdf) 
screen_ref(refGIS=refdf, phab= phabdf, cond = phabdf, natGIS = natdf) 





report<-screen_ref(refGIS=refdf) 
report %>%
   filter(StationCode=="514PS0206") %>%
  left_join(refdf %>% select(StationCode, AGUR_WS_01)) %>%
  select(AGUR_WS_01,AGUR_WS_01_pass)
