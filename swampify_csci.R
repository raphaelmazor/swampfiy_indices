library(tidyverse)

#Many fields require manual filling. These are left as blank ("") for manual entry in Excel.
swampify_CSCI<-function(x){
  #Core, Suppl1_mmi, and Suppl1_grps
  all_but_oe<-x$core %>%
    left_join(x$Suppl1_mmi) %>%
    left_join(x$Suppl1_grps) %>%
    select(-MMI_Percentile, -OoverE_Percentile, -MMI_Score) %>%
    # names() %>%dput()
    pivot_longer(cols = c(
      #From core report
      Count,Number_of_MMI_Iterations,Number_of_OE_Iterations, Pcnt_Ambiguous_Individuals,Pcnt_Ambiguous_Taxa,
      E,Mean_O,OoverE,CSCI,CSCI_Percentile,
      #From Suppl1_mmi
      Clinger_PercentTaxa, Clinger_PercentTaxa_predicted, Clinger_PercentTaxa_score, 
      Coleoptera_PercentTaxa, Coleoptera_PercentTaxa_predicted, Coleoptera_PercentTaxa_score, 
      Taxonomic_Richness, Taxonomic_Richness_predicted, Taxonomic_Richness_score, 
      EPT_PercentTaxa, EPT_PercentTaxa_predicted, EPT_PercentTaxa_score, 
      Shredder_Taxa, Shredder_Taxa_predicted, Shredder_Taxa_score, 
      Intolerant_Percent, Intolerant_Percent_predicted, Intolerant_Percent_score,
      #From Suppl1_grps
      pGroup1, pGroup2, pGroup3, pGroup4, pGroup5, pGroup6, pGroup7, pGroup8, pGroup9, pGroup10, pGroup11
    ),
    names_to = "AnalyteName",
    values_to = "Result") %>%
    mutate(AnalyteName = case_when(AnalyteName=="CSCI_Percentile"~"CSCI_Percentile", #The only variable that doesn't follow this naming convention
                                   T~paste0("CSCI_", AnalyteName)))
  #Do Suppl1_OE separately because it's already in long format and because it requires different analyte renaming.
  just_oe<-x$Suppl1_OE %>%
    select(StationCode, SampleID, AnalyteName=OTU, Result=CaptureProb) %>%
    mutate(AnalyteName=paste0("CSCI_Pc_",AnalyteName))
  #Bind, add other SWAMP fields, and re-order
  xdf<-bind_rows(all_but_oe, just_oe) %>%
    mutate(SampleDate="",
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
  xdf
}

###EXAMPLE
#generate CSCI results
library(CSCI)
example(CSCI)

results_swampified<-  swampify_CSCI(results) 
