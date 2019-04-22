get_hubs<-function(x,y){
  list<-paste("kME", y, sep = "") ##make module names match module membership names
  sig_mod_members<-x[,list] ## subset of only significant modules
  hubs<-sapply(1:dim(sig_mod_members)[2], #column index passed as variable a in function
               function(a,b) ##apply function with two arguments
                 rownames(b[order(b[,a],#row names, where b is data frame, a is clumn index
                                  decreasing = T),]), ##highest membership first
               b=sig_mod_members)%>%as.data.frame() #b is data frame
  colnames(hubs)<-list #set column names as signicant module colors
  hubs #return hubs for each
  }
## takes two arguments.
##X is the gene module membrership from WGCNA, where column names are "kME" followed by module color.
##Each column in X should have module membership for each gene
##Y is the list of significant module colors
##
