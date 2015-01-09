# The is the script of Exploratory Data Analysis
 library(epical)
 library(ggplot2)
 library(dplyr)
 library(plyr)
 library(lubridate)
 
zap()
data <- read.csv(
        file = "SKEPsurvey.csv", 
        header = TRUE,
        stringsAsFactor= FALSE)

 names(data) <- tolower(names(data))
 
 use(data)
 
 data[data == "-"] <- NA # replace '-' with NA
 str(data)

# the dataframe is not in the right type of variable, so it is needed to change some varible from chr to num

 data <- transform(data, 
                   phase = as.character(phase),
                   fno  = as.character(fno),
                   identifier = as.character(identifier),
                   country = as.character(country),
                   year = as.character(year),
                   season  = as.character(season),   
                   lat = as.character(season),
                   long = as.character(long),      
                   village = as.character(village), 
                   fa = as.numeric(fa),
                   fn = as.character(fn),
                   lfm = as.character(lfm),
                   pc = as.character(pc),
                   fp = as.character(fp),        
                   cem = as.character(cem),     
                   ast = as.character(ast),       
                   nplsqm = as.numeric(nplsqm),
                   ced = as.character(ced),       
                   cedjul = as.numeric(cedjul),
                   hd = as.character(hd),
                   hdjul = as.numeric(hdjul),     
                   ccd = as.character(ccd),
                   cvr = as.character(cvr),
                   vartype = as.character(vartype),
                   varcoded = as.character(varcode),
                   fym = as.character(fym),
                   fym.coded = as.character(fym.code),
                   n = as.numeric(n),
                   p = as.numeric(p) ,
                   k = as.numeric(k),
                   mf = as.numeric(mf),        
                   wcp = as.character(wcp),      
                   mu = as.character(mu) ,     
                   iu = as.numeric(iu),     
                   hu = as.numeric(hu),      
                   fu = as.numeric(fu),      
                   cs  = as.character(CS),      
                   ldg  =  as.numeric(ldg),  
                   yield = as.numeric(yield) ,
                   dscum = as.numeric(dscum),   
                   wecum = as.numeric(wecum),   
                   ntmax = as.numeric(ntmax), 
                   npmax = as.numeric(npmax),    
                   nltmax = as.numeric(nltmax),  
                   nlhmax = as.numeric(nltmax),  
                   waa = as.numeric(waa),      
                   wba = as.numeric(wba) ,   
                   dhx =  as.numeric(dhx),  
                   whx =  as.numeric(whx),     
                   ssx  = as.numeric(ssx),  
                   wma = as.numeric(wma), 
                   lfa = as.numeric(lfa),
                   lma = as.numeric(lma),   
                   rha  = as.numeric(rha) ,
                   thrx = as.numeric(thrx),    
                   pmx = as.numeric(pmx),    
                   defa  = as.numeric(defa) ,
                   bphx = as.numeric(bphx),   
                   wbpx = as.numeric(wbpx),    
                   awx  = as.numeric(awx), 
                   rbx =as.numeric(rbx),   
                   rbbx = as.numeric(rbbx),  
                   glhx  = as.numeric(glhx), 
                   stbx=as.numeric(stbx),    
                   rbpx =as.numeric(rbpx), 
                   hbx=as.numeric(hbx),
                   bbx =as.numeric(bbx),    
                   blbx =as.numeric(bibx),    
                   lba = as.numeric(lba),    
                   bsa =as.numeric(bsa),    
                   blsa = as.numeric(blsa),  
                   nbsa = as.numeric(nbsa),  
                   rsa  =as.numeric(rsa),   
                   lsa =as.numeric(lsa),    
                   shbx = as.numeric(shbx) ,  
                   shrx =as.numeric(shrx),    
                   rx= as.numeric(rx),    
                   fsmx =as.numeric(fsmx),   
                   nbx =  as.numeric(nbx),   
                   dpx =as.numeric(dpx),    
                   rtdx  =as.numeric(rtdx),  
                   rsdx  = as.numeric(rsdx),
                   gsdx  =as.numeric(gsdx),   
                   rtx =as.numeric(rtx),
 ) 
 label.var(phase, "SKEP Phase")
 label.var(fno, "Field Number")
 label.var(identifier, "Code" )
 label.var(country, "country")
 label.var(year, " Year") 
 label.var(season, " Season")    
 label.var(lat, " Longitude") 
 label.var(long, "longitude")    
 label.var(village, "Village") 
 label.var(fa, "Filed area") 
 label.var(fn, "Farmer name") 
 label.var(lfm, " land form") 
 label.var(pc, " Previous crop")
 label.var(fp, "Fallow period")  
 label.var(cem, "Crop Establisment method") 
 label.var(ast, "Age of seedlings at transplanting") 
 label.var(nplsqm, "No of plant per square mater")
 label.var(ced , 'Crop Establisment Date'
 label.var(cedjul, 'Crop Establisment date in Julian day'
 label.var(hd , 'Harvest Date'
 label.var(hdjul, 'Harvest Date in Julian day'
 label.var(ccd , 'Crop Cycle duration'
 label.var(cvr, 'Cultivar or vaiety'
 label.var(vartype , 'Variety type code 1 (modern), 2(traditional), 3(hybrid)'
 label.var(varcoded, 'Variety type code 1 (modern), 2(traditional), 3(hybrid)'
 label.var(fym, 'Farm Yard manure')
 label.var(fym.coded, 'Farm Yard manure code, 1 applied, 2 = no applied')
 label.var(n, ' amount of Nitrogen') 
 label.var(p , 'amount of Phosphorus')
 label.var(k, "amount of Potessium"
 label.var(mf, 'total amount of NPK')
 label.var(wcp, 'weed control practice')
 label.var(mu, 'molluscicide use or application')
 label.var(iu, 'insecticide use or application')  
 label.var(hu , 'herbicide use or application')
 label.var(fu , 'funcidie use or application')
 label.var(cs , 'crop status vist')
 label.var(ldg, 'lodging %')
 label.var(yield, 'Yield at 14% moisture content t/ha')
 label.var(dscum, 'Cumulative drought stress')
 label.var(wecum, ''
 label.var(ntmax 
 label.var(npmax 
 label.var(nltmax 
 label.var         nlhmax 
 label.var         waa 
 label.var          wba 
 label.var         dhx 
 label.var         whx  
 label.var         ssx 
 label.var          wma 
 label.var         lfa 
 label.var          lma 
 label.var         rha  
 label.var         thrx     
 label.var         pmx 
 label.var          defa  
 label.var         bphx 
 label.var          wbpx 
 label.var          awx  
 label.var          rbx 
 label.var         rbbx 
 label.var         glhx  
 label.var         stbx
 label.var         rbpx 
 label.var         hbx
 label.var          bbx
 label.var         blbx 
 label.var          lba     
 label.var          bsa 
 label.var          blsa 
 label.var           nbsa 
 label.var         rsa   
 label.var         lsa 
 label.var         shbx  
 label.var          shrx    
 label.var           rx
 label.var          fsmx 
 label.var          nbx 
 label.var          dpx  
 label.var          rtdx  
 label.var          rsdx 
 label.var          gsdx  
 label.var           rtx 
