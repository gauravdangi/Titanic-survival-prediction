
title_fun=function(name1){
  
  if(grepl("Mr. ",name1)){
    
    return("Mr. ")
  }
  
  else if(grepl("Mrs. ",name1)){
    return("Mrs. ")
  }
  
  else if(grepl("Miss. ",name1)){
    
    return("Miss. ")
  }
  
  else if(grepl("Master. ",name1)){
    
    return("Master. ")
  }
  else{
    
    return("Others ")
  }
  
}