###############################################################################
#'title         : Cleaning survey data on SKEP Phase I
#'date          : July, 2014
#'purpose       : load data from the shared Google Drive which is the exel 
#'                format
#'writed by     : Sith Jaisong (s.jaisong@irri.org)
#'contact       : International Rice Research Institute
#'input         : data frame named ds from the previous step (1.load data)
#'output        : data frame and RData 
###############################################################################
#Load data saved from the 1.load.data 
lnames <- load("survey.data.RData")
lnames
#####---- Set up the all varibles in the right class 
## ID variable such as the field no, identifier
id <- c("fno", # the field number
        "identifier",
        "year", #code for identifier
        "season"
)

# location
location <- c("latitude", # Latitude in decimal degree
              "latitude_rev",
              "longitude", # Longitude in decimal degree
              "longitude_rev",
              "village",# village name
              "country"
)

## Farmer information such as size 
farmer.info <- c("fa", # field area
                 "fn", # file name
                 "lfm", # type of rice 
                 "pc", # previous crop
                 "fp", # fellow period
                 "cem", # crop establishment
                 "ast", #
                 "nplsqm", #number of leaf per square mater
                 "ced", # crop establisment date
                 "cedjul", 
                 "hd", # harvest date
                 "hdjul",
                 "ccd",
                 "cvr", #cutivar
                 "vartype", #varitiety type
                 "varcoded",# rice variety code
                 "fym",
                 "fym_coded"
                 )
dvs <- c("dvs_1",# development stage at the first visit 
         "dvs_2" # development stage at the second visit
         )

pro.sit <- c("n", # amount of Nitrogen applied (kg/ha)
             "p", # amount of Phosphorus applied (kg/ha)
             "k", # amount of Potassium applied (kg/ha)
             "mf", # amount of Manure fertilizer applied (kg/ha)
             "iu", # frequency of insecticide applied (kg/ha)
             "hu", # frequency of herbicide applied (kg/ha)
             "fu" # frequency of funcidie applied (kg/ha)
                )
insect.injuries <- c("dh", # dead heart
                "wh", # white head
                "gm", # gall midge
                "rt", # rat damage
                "wm", # whorl maggot
                "lf", # leaf foldder
                "def", # defoliators
                "bph", # brown plant hopper
                "wbp", # white back brown plant hopper
                "aw", # 
                "rb", #
                "rbb", #
                "glh" #
)
disease <-c("blb", # bacterial leaf blight
                "lb", # leaf blast
                "bs", # brown spot
                "bls", # bacterial leaf streak
                "nbs", # nerrow brown spot
                "rs", # red stripe
                "ls", # leas streak
                "shb", # sheath blight
                "shr", # sheath rot
                "sr", # 
                "fsm", # false smut
                "nb", # neck blast
                "dp", # dirty panicle
                "rtd", # rice tungro disease
                "rsd", # ragged stunt disease
                #"gsd", # gressy stunt disease No data
                #"hb", # No data
                "bu",
                "mu",
                "pghu",
                #"stb", # No data        
                "de"
                )
yield <- c("yield")
#-----------------------------------------------------------------------------#

#####-----select the varibles that not involve in the analysis-----####
ignore <- c(id, location, farmer.info, dvs) 

#####-----select the varibles for the analysis-----####
vars <- c("country",pro.sit, insect.injuries, disease)

## clean data bacase the data must be the numeric data

selected.data <- ds[vars]
yield <- ds[c("country", "yield")]

#selected.data <- selected.data[complete.cases(selected.data),] # delete the rows that contain NA 
#crop.injur <- selected.data[c("country", pro.sit, insect.injuries, disease)]
#yield <- selected.data[c("country","yield")]

save(selected.data, file = "selected.data.RData")
#eos