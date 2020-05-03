library(tidyverse)

#Many fields require manual filling. These are left as blank ("") for manual entry in Excel.
swampify_ASCI<-function(x){
  #Core, Suppl1_mmi, and Suppl1_grps
  xdf<-x %>%
    
    select(-SampleType, -UnrecognizedTaxa) %>%
    # names() %>%dput()
    pivot_longer(cols = c(
      c(D_ValveCount, S_EntityCount, S_Biovolume, D_NumberTaxa, 
        S_NumberTaxa, H_NumberTaxa, D_ASCI, S_ASCI, H_ASCI, 
        D_pct_att_prp_spp_BCG12, D_pct_att_prp_spp_OxRq_DO100_75, 
        D_pct_att_prp_spp_Salinity_BF, D_pct_att_prp_spp_Trophic_E, 
        D_prp_spp_BCG12_mod, D_prp_spp_BCG12_mod_scr, D_prp_spp_BCG12_pred, 
        D_prp_spp_OxRq_DO100_75_raw, D_prp_spp_OxRq_DO100_75_raw_scr, 
        D_prp_spp_Salinity_BF_mod, D_prp_spp_Salinity_BF_mod_scr, 
        D_prp_spp_Salinity_BF_pred, D_prp_spp_Trophic_E_mod, D_prp_spp_Trophic_E_mod_scr, 
        D_prp_spp_Trophic_E_pred, H_OxRd_DO_30_richness_mod, H_OxRd_DO_30_richness_mod_scr, 
        H_OxRd_DO_30_richness_pred, H_pct_att_OxRd_DO_30_richness, 
        H_pct_att_prp_spp_BCG4, H_pct_att_prp_spp_IC_DOC_high, H_pct_att_Salinity_BF_richness, 
        H_prp_spp_BCG4_mod, H_prp_spp_BCG4_mod_scr, H_prp_spp_BCG4_pred, 
        H_prp_spp_IC_DOC_high_raw, H_prp_spp_IC_DOC_high_raw_scr, 
        H_Salinity_BF_richness_mod, H_Salinity_BF_richness_mod_scr, 
        H_Salinity_BF_richness_pred, S_cnt_spp_IC_DOC_high_raw, S_cnt_spp_IC_DOC_high_raw_scr, 
        S_pct_att_cnt_spp_IC_DOC_high, S_pct_att_prp_spp_BCG45, S_pct_att_prp_spp_Green, 
        S_prp_spp_BCG45_raw, S_prp_spp_BCG45_raw_scr, S_prp_spp_Green_raw, 
        S_prp_spp_Green_raw_scr)),
    names_to = "AnalyteName",
    values_to = "Result") %>%
    mutate(AnalyteName =case_when(AnalyteName =="D_ASCI"~"ASCI_D",
                                  AnalyteName =="S_ASCI"~"ASCI_S",
                                  AnalyteName =="H_ASCI"~"ASCI_H",
                                  T~paste0("ASCI_", AnalyteName)),
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
           Replicate=Replicate,
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
           HabitatResultComments=Comments,
           
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
library(ASCI)
example(ASCI)
results<-ASCI(tmp, demo_station)


results_swampified<-  swampify_ASCI(results) 
