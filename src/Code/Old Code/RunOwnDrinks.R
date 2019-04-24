# Estimate Own Price Elasticities for Drinks
source("Source.R")
# Add lags
# Select top products
# Run logprice reg and logsales reg
res_fs<-fs_jjfoods(run_fs=FALSE)
# Average month elasticity
het<-"month"
subset.name<-""
subset<-NULL
main(res_fs,het,outname=paste0(outname_list[[1]],het),xbreaks=month.abb)
# Average month elasticity for Soft Drinks
het<-"month"
subset.name<-"SoftDrinks"
subset<-res_fs$fe_pdata[,"Level2166"]==1
main(res_fs,het,subset=subset,subset.name=subset.name,outname=paste0(outname_list[[1]],het),xbreaks=month.abb)

# Average month elasticity for Water
het<-"month"
subset.name<-"Water"
subset<-res_fs$fe_pdata[,"Level2188"]==1
main(res_fs,het,subset=subset,subset.name=subset.name,outname=paste0(outname_list[[1]],het),xbreaks=month.abb)

# Average Level2 elasticity
het<-"^(?=.*Level2|Level1)(?!.*lag)"
subset<-NULL
subset.name<-""
xbreaks<-xbreaks[[1]]
main(res_fs,het,outname=outname_list[[1]],xbreaks=xbreaks)


