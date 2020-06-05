library(tidyverse)


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
           MatrixName="habitat", #DOES THIS MAKE SENSE?
           MethodName=paste0("PHAB_software_v",packageVersion("PHAB")),
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
           HabitatResultComments) %>%
    mutate(UnitName = case_when(AnalyteName %in% c("PCT_SAFN","PCT_RC","IPI_PCT_SAFN_pred","XCMG","XSLOPE","XCDENMID","IPI_XCMG_pred",
                                                "PCT_FAST","PCT_SLOW","PCT_POOL","PCT_DR")~"%",
                                AnalyteName %in% c("IPI_IPI_qa","IPI_Ev_FlowHab_qa","IPI_H_AqHab_qa","IPI_H_SubNat_qa","IPI_PCT_SAFN_qa","IPI_XCMG_qa")~"count",
                                AnalyteName %in% c("X_BKFW")~"m",
                                AnalyteName %in% c("IPI","IPI_Ev_FlowHab_score","IPI_H_AqHab_score","IPI_H_SubNat_score","IPI_PCT_SAFN_score","IPI_XCMG_score")~"score",
                                AnalyteName %in% c("Ev_FlowHab","H_AqHab","IPI_H_AqHab_pred","H_SubNat","XCMGW","XFC_NAT_SWAMP","W1_HALL_SWAMP")~"none",
                                T~"error"))
  xdf
}

###EXAMPLE
#generate IPI results
library(PHAB)
example(IPI)
results<-IPI(stations, phab)

#If you only have the results
results_swampified<-  swampify_IPI(results) 

#If you also have the inputs
results_swampified<-  swampify_IPI(res=results, inp=phab) 

