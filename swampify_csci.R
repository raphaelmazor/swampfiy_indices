library(tidyverse)
#Only covers core results for now
#Many fields require manual filling. These are left as blank ("") for manual entry in Excel.

swampify_CSCI<-function(x){
  core<-x$core %>%
    select(StationCode,
           SampleID,
           Count,
           Number_of_MMI_Iterations,
           Number_of_OE_Iterations, Pcnt_Ambiguous_Individuals,
           Pcnt_Ambiguous_Taxa,
           E,
           Mean_O,
           OoverE,
           CSCI,
           CSCI_Percentile) %>%
    pivot_longer(cols = c(Count,
                          Number_of_MMI_Iterations,
                          Number_of_OE_Iterations, Pcnt_Ambiguous_Individuals,
                          Pcnt_Ambiguous_Taxa,
                          E,
                          Mean_O,
                          OoverE,
                          CSCI,
                          CSCI_Percentile),
                 names_to = "AnalyteName",
                 values_to = "Result") %>%
    mutate(AnalyteName = case_when(AnalyteName=="CSCI_Percentile"~"CSCI_Percentile",
                                   T~paste0("CSCI_", AnalyteName)),
           SampleDate="",
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
           MatrixName="benthic",
           MethodName=paste0("CSCI_software_v",packageVersion("CSCI")),
           FractionName="None",
           UnitName="none",
           VariableResult="",
           ResQualCode="=",
           QACode="None",
           ComplianceCode="Pend",
           BatchVerificationCode="NR",
           CollectionDeviceName="D-Frame Kick Net", #Or do we want to leave blank?
           HabitatResultComments=""
           
    ) %>%
    select(StationCode, SampleID, SampleDate, 
           ProjectCode, EventCode, ProtocolCode, AgencyCode, SampleComments, 
           LocationCode, GeometryShape, CollectionTime, CollectionMethodCode, 
           Replicate, HabitatCollectionComments, MatrixName, MethodName, AnalyteName,
           FractionName, UnitName, VariableResult, Result, ResQualCode, 
           QACode, ComplianceCode, BatchVerificationCode, CollectionDeviceName, 
           HabitatResultComments)
  bind_rows(core)
  
}

###EXAMPLE
#generate CSCI results
library(CSCI)
example(CSCI)

results_swampified<-swampify_CSCI(results)

