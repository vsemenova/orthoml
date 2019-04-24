own_food<-function(k) {
  # Select Level1 categories
  level1_inds<<-level1_inds_list[[k]]
  source("Source.R")
  
  res_fs<-fs_jjfoods(run_fs=FALSE)
  # Average Month Elasticity
  het<-"month"
  main(res_fs,het,outname=paste0(outname_list[[k]],het),xbreaks=month.abb)
  # Average Category Elasticity
  het<-"^(?=.*Level1)(?!.*lag)"
  outname<-paste0("Level1",outname_list[[k]])
  res_ss_b<-main(res_fs,het,outname=outname,xbreaks=xbreaks[[k]])
  # Average Category Elasticity at Level2
  het<-"^(?=.*Level2|Level1)(?!.*lag)"
  toplevel<-"Level1"
  bottomlevel<-"Level2"
  outname<-paste0("Level2",outname_list[[k]])
  main(res_fs,het,outname=outname,legend_breaks=xbreaks[[k]],toplevel=toplevel,bottomlevel=bottomlevel,maxy=15,mu_bar=0.1)
  
  # Average Category Elasticity at Level3
  het<-"^(?=.*Level3|Level2|Level1)(?!.*lag)"
  toplevel<-"Level1"
  bottomlevel<-"Level3|Level2"
  outname<-paste0("Level3",outname_list[[k]])
  res_ss_b<-main(res_fs,het,outname=outname,legend_breaks=xbreaks[[k]],toplevel=toplevel,bottomlevel=bottomlevel,maxy=25,mu_bar=0.5)

   if (k!=3) {
     # No OLS for NonEdibles at Level4
     # Average Category Elasticity at Level4
     het<-"^(?=.*Level4|Level3|Level2|Level1)(?!.*lag)"
     toplevel<-"Level1"
     bottomlevel<-"Level4|Level3|Level2"
     outname<-paste0("Level4",outname_list[[k]])
     res_ss_b<-main(res_fs,het,outname=outname,legend_breaks=xbreaks[[k]],toplevel=toplevel,bottomlevel=bottomlevel,maxy=30,mu_bar=0.9)
     
   }
 
}


