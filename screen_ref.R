

library(tidyverse)

screen_ref<-
  function(GIS=GIS, nlcd_year=2016, phab=NULL) {
    GIS2<-GIS %>%
      transmute(StationCode=StationCode,
              CNL_PI_PCT_pass = CNL_PI_PCT < 10,
              MINES_pass = MINES==0,
              # NRST_DAM_pass = NRST_DAM > 10,
              
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
              CD21_1k_01_pass= CD21_1k_01  < 10,
              AG_1k_06_pass = AG_1k_06 < 3,
              UR_1k_06_pass = UR_1k_06 < 3,
              AGUR_1k_06_pass = AGUR_1k_06 < 5,
              CD21_1k_06_pass= CD21_1k_06  < 10,
              AG_1k_16_pass = AG_1k_16 < 3,
              UR_1k_16_pass = UR_1k_16 < 3,
              AGUR_1k_16_pass = AGUR_1k_16 < 5,
              CD21_1k_16_pass= CD21_1k_16  < 10,
              
              AG_5k_01_pass = AG_5k_01 < 3,
              UR_5k_01_pass = UR_5k_01 < 3,
              AGUR_5k_01_pass = AGUR_5k_01 < 5,
              CD21_5k_01_pass= CD21_5k_01  < 10,
              AG_5k_06_pass = AG_5k_06 < 3,
              UR_5k_06_pass = UR_5k_06 < 3,
              AGUR_5k_06_pass = AGUR_5k_06 < 5,
              CD21_5k_06_pass= CD21_5k_06  < 10,
              AG_5k_16_pass = AG_5k_16 < 3,
              UR_5k_16_pass = UR_5k_16 < 3,
              AGUR_5k_16_pass = AGUR_5k_16 < 5,
              CD21_5k_16_pass= CD21_5k_16  < 10,
              
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
        if(nlcd_year==2016)
        {
          myvars2<-c(myvars, "AG_WS_16_pass","UR_WS_16_pass", "UR_WS_16_pass", "CD21_WS_16_pass",
                     "AG_5k_16_pass","UR_5k_16_pass", "UR_5k_16_pass", "CD21_5k_16_pass",
                     "AG_1k_16_pass","UR_1k_16_pass", "UR_1k_16_pass", "CD21_1k_16_pass")
        }
    
    GIS2 %>% 
      select(StationCode, all_of(myvars2)) 
    GIS3 <-GIS2 %>%
      pivot_longer(cols=all_of(myvars2), names_to="metric", values_to = "screen") %>%
      group_by(StationCode) %>%
      summarise(Ref_GIS_Ode2016 = all(screen),
                Ref_GISfailed_Ode2016 = sum(!screen)) %>%
      ungroup()
    GIS4<-inner_join(GIS2,GIS3)
    if(is.null(phab))
    {
      GIS4
    } else
      {
    PHABmets<-GIS %>%
      select(StationCode) %>%
      inner_join(phab %>%
                   select(StationCode, SampleDate, Code, Result) %>%
                   filter(Code=="W1_HALL_SWAMP")
                   ) %>%
      group_by(StationCode) %>%
      summarise(maxW1 =max(Result, na.rm=T)) %>%
      transmute(StationCode=StationCode,
                W1_HALL_SWAMP_pass=maxW1<1.5)
    GIS5<-left_join(GIS4, PHABmets) %>%
      mutate(Ref_GIS_Field_Ode2016= case_when(is.na(W1_HALL_SWAMP_pass) | is.na(Ref_GIS_Ode2016)~NA ,
                                              T~(Ref_GIS_Ode2016 & W1_HALL_SWAMP_pass )))
    GIS5}
    }

refdf<-read.csv("RefScreening/refgis.csv", stringsAsFactors = F) %>% 
  mutate(NRST_DAM = 9999)

phabdf<-read.csv("RefScreening/ReportingMetrics.csv", stringsAsFactors = F)

    
screen_ref(GIS=refdf)

screen_ref(GIS=refdf, phab=phabdf)
