library(tidyverse)

results

#Many fields require manual filling. These are left as blank ("") for manual entry in Excel.
swampify_IPI<-function(res, inp=NULL){
  #Core, Suppl1_mmi, and Suppl1_grps
  if(missing(inp))
    xdf<-res
  else
    xdf<-res %>%
      left_join(
        inp %>% 
          filter(Variable %in% c("XSLOPE", "XBKFW","XCGMW","XFC_NAT_SWAMP",
                                 "XCDENMID","PCT_FAST","PCT_SLOW","PCT_POOL","PCT_DR","W1_HALL_SWAMP")) %>%
          pivot_wider(id_cols=c(StationCode, SampleDate,SampleAgencyCode),
                      names_from=Variable, values_from=Result)
          
        
      )
  xdf<- xdf %>%
    
    select(-IPI_percentile) %>%
    # names() %>%dput()
    pivot_longer(cols = c(
      c(IPI, 
        Ev_FlowHab, Ev_FlowHab_score,
        H_AqHab, H_AqHab_pred, H_AqHab_score,
        H_SubNat, H_SubNat_score, 
        PCT_SAFN, PCT_RC, PCT_SAFN_pred, PCT_SAFN_score,
        XCMG, XCMG_pred, XCMG_score, 
        IPI_qa, Ev_FlowHab_qa, H_AqHab_qa, H_SubNat_qa, PCT_SAFN_qa, XCMG_qa)),
      names_to = "AnalyteName",
      values_to = "Result") %>%
    mutate(AnalyteName =case_when(AnalyteName %in% c("IPI",
                                                     "Ev_FlowHab","H_AqHab","H_SubNat","PCT_SAFN","PCT_RC","XCMG",
                                                     "XSLOPE", "XBKFW","XCGMW","XFC_NAT_SWAMP", "XCDENMID","PCT_FAST","PCT_SLOW","PCT_POOL","PCT_DR","W1_HALL_SWAMP") ~AnalyteName,
                                  T~paste0("IPI_", AnalyteName)),
           SampleDate=SampleDate,
           ProjectCode="",
           EventCode="BA",
           ProtocolCode="", #Or always "SWAMP_2016_WS"?
           AgencyCode="",
           SampleComments="",
           LocationCode="X",
           GeometryShape="Point",
           CollectionTime="",
           CollectionMethodCode="",
           Replicate="",
           HabitatCollectionComments="",
           MatrixName="benthic", #DOES THIS MAKE SENSE?
           MethodName=paste0("PHAB_software_v",packageVersion("CSCI")),
           FractionName="None",
           UnitName="none",
           VariableResult="",
           ResQualCode="=",
           QACode="None",
           ComplianceCode="Pend",
           BatchVerificationCode="NR",
           CollectionDeviceName="", 
           HabitatResultComments="",
           
    ) %>%
    select(StationCode, PHAB_SampleID, SampleDate, 
           ProjectCode, EventCode, ProtocolCode, AgencyCode, SampleComments, 
           LocationCode, GeometryShape, CollectionTime, CollectionMethodCode, 
           Replicate, HabitatCollectionComments, MatrixName, MethodName, AnalyteName,
           FractionName, UnitName, VariableResult, Result, ResQualCode, 
           QACode, ComplianceCode, BatchVerificationCode, CollectionDeviceName, 
           HabitatResultComments)
  xdf
}

###EXAMPLE
#generate CSCI results
library(PHAB)
example(IPI)
results<-IPI(stations, phab)
head(phab)


results_swampified<-  swampify_IPI(results) 
