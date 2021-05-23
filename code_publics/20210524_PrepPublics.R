## =================
# Rémi code for the creation of users profiles
# created: 25 Nov 2019
# updated: 14 Feb 2020

library(dplyr)
library(reshape2)
library(stringr)
library(ggplot2)
library(vegan)
library(readxl)
library(quanteda)
library(tidyr)
library(tidytext)
library(tidyverse, warn.conflicts = FALSE)
library(scales)
library(knitr)

## =================
# load users info
## =================
setwd("")
users <- read_excel("", sheet="User_totals", col_names = TRUE)
userspapers <- read_excel("", sheet="Users_papers", col_names = TRUE)

## =================
# load papers info
## =================
papers <- read_excel("", sheet = "Tweets_papers",  col_names = TRUE)
authors <- read_excel("", sheet = "Authors",  col_names = TRUE)
authors <- rename(authors, "id_art" = "ID_Art")

##==================
# create table for data analysis
##==================
usersallinfo <- left_join(users, userspapers %>% select(ID_Art, Author_id_on_Source), by = "Author_id_on_Source")
usersallinfo <- rename(usersallinfo, "id_art" = "ID_Art")
usersallinfo <- left_join(usersallinfo, papers %>% select(id_art, titre, abstract, Revue, NSFDisc, Nb_Page, Nb_Auteur, tweets, users, tweetspan), by = "id_art")
usersallinfo$id_art <- as.character(as.numeric(usersallinfo$id_art))
users <- left_join(usersallinfo, authors %>% select(id_art, Annee, Prenom, Nom_famille), by = "id_art")
users <- users %>%
  filter(!is.na(id_art))
users <- users %>%
  mutate(userint = if_else(
    users < 11, "1-10", (
      if_else(users < 21, "11-20", 
        if_else(users < 31, "21-30", 
          if_else(users < 41, "31-40", 
            if_else(users < 51, "41-50", 
              if_else(users < 61, "51-60", 
                if_else(users < 71, "61-70", 
                  if_else(users < 81, "71-80", 
                    if_else(users < 91, "81-90", 
                      if_else(users < 101, "91-100", 
                        if_else(users > 100, "101+", ""
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

users %>%
  count(userint)


## =================
# basic string cleaning
## =================

#remove capitals
users$description <- tolower(users$Author_Description)

#remove urls
users$description <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", users$description)

#remove emojis
users$description <- gsub("[^\x01-\x7F]", "", users$description)

#remove numbers
users$description <- gsub("[0-9]+", "", users$description)

#remove lists with / and replace with " " (e.g., wife/phd/friend)
users$description <- gsub("\\/", " ", users$description)

# forward slash -- escape with a double backslash
sub("\\/", " ", "Peace/Love")
#[1] "Peace Love"

length(unique(users$description))

#remove punctuations
punct <- '[]\\?!\"\'$%&(){}+*/:;,._`|~\\[<=>\\^-]'
users$description <- gsub(punct, "", users$description)

#remove NULL
users <- users %>% 
  filter(!grepl('NULL', Author_Description))

users <- users %>% 
  filter(!is.na(description))

## =================
#code to academia
## =================
#lectur*, prof* 

sci <- c("\\blectur(er|ing)\\b", 
         "\\bprof\\b", "\\bprofess(o|eu)re?\\b", 
         "\\b(ph|m)\\s?(d|sc)i?\\b", 
         "\\bpost\\s?doc(\\w+)?\\b", 
         "\\bdoctora(l|nt)\\b", 
         "\\bstudent\\b", 
         "\\bresearch(er|ing|\\s(fellow|assistant|associate|leader|director))\\b", 
         "\\bi\\s(research|study)\\b",
         "\\bscient(i(st|fique))?\\b",
         "\\bchair\\b", "\\bassociate\\sdirector\\b", "\\b((micro)?bio|(e|é)co|pala?eonto|viro|onco|hema|epidemio|physio|archa?eo|anthropo|psycho|patho|endocrino|nephro|limno|socio|neuro|geo|physic|botan|entomo|hydro|genetic|chem|ethic)(log)?(u|ist|ien(ne)?)e?\\b", 
         "\\b(\\w+)?grad(\\w+)?\\b", 
         "\\borcid\\b", 
         "\\b(bachelor|master)\\sdegree\\b", 
         "\\bdean\\b", 
         "\\bhistori(e|a)n(ne)?\\b", 
         "\\bfaculty\\b", "\\bscholar\\b", 
         "\\b(g(e|é)|oc(e|é)an)ographer?\\b", 
         "\\b#sci(comm|art|engage)\\b", 
         "\\bprgrm\\b", 
         "\\bacad(é|e)mi(c|que)\\b", 
         "\\bétudiante?\\b", 
         "\\bchercheure?\\b")

testsci <- c("lecturer in marine","university professor",
           "journal of the","university of X biology phd student",
           "project","professora", "ph d", "post doc", "postdoc", "doctoral", "doctorant", "researcher", "researchers", "researching", "i research", "research associate", "research director", "fellow", "research project", "i study", "scientist", "lecturing", "scientifique", "associate director", "biologist", "écologiste", "ecologist", "microbiologiste", "archaeologist", "sociologist", "sociologue", "physicist", "physicien", "physicienne", "entomologiste", "botaniste", "hydrologist", "physicists", "graduate", "postgrad", "master degree", "faculty", "dean", "msc", "m sc", "postdoctoral", "scient", "historienne", "géographe", "#scicomm", "msci", "académique", "professeur", "étudiante", "chercheur")

grepl(paste(sci, collapse="|"),testsci)

## =================
#code to comm
## =================
#journalist*, reporter*

comm <- c("\\bwr(i|o)t(e(r?|s?)|ing)\\b", 
          "\\baut(hor|eur)\\b", 
          "\\bjourn(o|aliste?)\\b", 
          "\\bblog(ger|ging|s?\\s(for|on))?\\b", 
          "\\bcolumnist\\b", 
          "\\bcommunicat((o|eu)r|ion|ing)\\b", 
          "\\bfreelance.?\\swri.+\\b", 
          "\\b(repor|pr(e|é)sen|(e|é)di|(e|é)duca|contribu|produc|teach|storytell|commentat|broadcast|youtub|chroniq|speak)(t?a?t?((eu?|o)r|rice))\\b", 
          "\\bpodcast(er)?\\b", 
          "\\bcorrespond(e|a)nt\\b", 
          "\\bwebsite\\b", 
          "\\bfilms?\\b", 
          "\\bshow\\shost\\b", 
          "\\bdocu(mentary)?\\b", 
          "\\bsharing\\sscience\\b", 
          "\\bcameraman\\b", 
          "\\béditorialiste\\b", 
          "\\b@carbon\\sbrief\\b", 
          "\\bcurated\\sby\\b")
testcomm <- c("auteur", "writer", "co author", "journaliste", "journal", "blogs", "blog", "blogs for", "freelance writing", "freelance worker", "reporter", "podcaster", "write", "correspondent", "website", "educator", "producer", "éditrice", "présentatrice", "film", "show host", "communication", "docu", "youtuber", "writes", "writers", "@carbon brief")

grepl(paste(comm, collapse="|"),testcomm)

## =================
#code to organization
## =================

organ <- c("\\b(universit(y|é)|institute?|societ(y|é)|library|lab(o?(rato(ry|ire))?)|college|community|organi((z|s)ation|sme)|association|school|d(e|é)p(t|arte?ment)|program(me)?|faculté|cent(er|re)|team|groupe?|advocates|leaders|experts|branch|parternship|network|compag?n(y|i)e?|alliance|initiative|channel|specialists|hub|agency|office|bureau|charity|movement|founded|nonprofit|llc|club|courses|campus|station|collaboration|project|designs|inc|grouping|forum|citizens|consultants|committee|offici(a|eu?)(l|x)|toolkit)\\b", 
           "\\bjoin\\s(us|the)\\b", 
           "\"\bpromotes\\b", 
           "\\bour\\sgoal\\b", 
           "\\bwe\\s((sh)?a?re|want|campaign|deliver)\\b", 
           "\\blive\\sevents\\b", 
           "\\b(tweets|managed)\\sby\\b", 
           "\\bmail\\sat\\b", 
           "\\baims\\sto\\b",
           "\\bcoming\\ssoon\\b")
testorgan <- c("université", "society", "college", "département", "labo", "lab", "laboratoire", "centers", "organisme", "tweets by", "we share", "we are", "officiel")

grepl(paste(organ, collapse="|"),testorgan)

## =================
#code to bots
## =================

bot <- c("\\b(ro)?bot\\b", 
         "\\b(paper|publication|lit|preprint|article|peer-review|journal|news|data)\\s?feed\\b", 
         "\\brss\\b", 
         "\\bnew\\s(submissions?|papers?)\\b", 
         "\\b(latest|new)\\spublications?\\b", 
         "\\b(a|bio)rxiv\\b", 
         "\\bpubmed\\b", 
         "\\bpapers?\\s(auto|stream|tweet|updates?|links?|alerts?)", "\\bpublications?\\salerts?\\b", 
         "\\bdaily\\s(updates?|compilations?)\\b", 
         "\\bcompilations?\\b", 
         "\\bretweet(s|ed)?\\s(from|any)\\b")
testbot <- c("robot", "news feed", "datafeed", "new submissions", "paper alerts", "daily updates", "retweets from")

grepl(paste(bot, collapse="|"),testbot)

## =================
#code to professional
## =================

pro <- c("\\b(physi|pediatri)cian\\b", 
         "\\bdoct(eu|o)re?\\b", 
         "\\bprofession(a|e)l(le)?\\b", 
         "\\bnurse\\b", 
         "\\bnutritionist\\b", 
         "\\bc(e|t)o\\b", 
         "\\bvet(erinarian)?\\b", 
         "\\bconsult(ing|ante?)\\b", 
         "\\bofficer\\b", 
         "\\banalyste?\\b", 
         "\\bfarmer\\b", 
         "\\btrustee\\b", 
         "\\bdir(ect(o|eu)?r(ice)?)?\\b", 
         "\\b(é|e)conomiste?\\b", 
         "\\bentrepreneur\\b", 
         "\\bstewardship\\b", 
         "\\bstrat(e|è)g(e|ist)\\b", 
         "\\bwork(ing)?\\s(at|for)\\b", 
         "\\b(e|i)ng(ineer|énieure?)\\b", 
         "\\btranslator\\b", 
         "\\bltd\\b", 
         "\\bagriculteur\\b", 
         "\\bmanager\\b", 
         "\\blawyer\\b", 
         "\\bplanner\\b",
         "\\bd(é|e)velop(e|eu)r\\b", 
         "\\borganiser\\b", 
         "\\badvis(o|e)r\\b", 
         "\\battorney\\b", 
         "\\bcounsel\\b", 
         "\\bsecretary\\b", 
         "\\bself\\semployed\\b", 
         "\\bfisherman\\b", 
         "\\b(co\\s)?fou?nd(at)?eu?r(ice)?\\b", 
         "\\bconservationist\\b", 
         "\\bdesigner\\b", 
         "\\bhead\\sof\\b", 
         "\\binvestor\\b", 
         "\\bprincipal\\sat\\b", 
         "\\bcoord(o|i)nat(ing|(eu|o)?r(ice)?)\\b", 
         "\\bemployer\\b", 
         "\\bpaysan\\b", 
         "\\bpr(é|e)sidente?\\b", 
         "\\bbusiness\\s(owner|development)\\b", 
         "\\bspecialist\\b", 
         "\\bhandyman\\b", 
         "\\bsolicitor\\b", 
         "\\bsustainable\\sfinance\\b", 
         "\\bweb\\sdev\\b", 
         "\\bday\\sjob\\b", 
         "\\bchairman\\b", 
         "\\blibrarian\\b", 
         "\\bcoach\\b", 
         "\\bénergéticien\\b", 
         "\\bvp\\b", 
         "\\bexecutive\\b", 
         "\\barchitect\\b", 
         "\\bexperienced\\b")
testpro <- c("physician", "doctor", "public health professional", "directeur", "directrice", "analyste", "consultant", "ceo", "self-employed")

grepl(paste(pro, collapse="|"),testpro)

## =================
#code to personal
## =================

pers <- c("\\bwife\\b", 
          "\\bmale\\b", 
          "\\b(grand)?(m|p|f|d)(o|u|a)(d|m(my)?|ther)?\\b", 
          "\\bphotographer?\\b", 
          "\\bwhite\\b", 
          "\\bfood\\b", 
          "\\bgeek\\b", 
          "\\bhusband\\b", 
          "\\bmusic\\b", 
          "\\brunn(er|ing)\\b", 
          "\\b(m|p)ère\\b", 
          "\\bmarr?i(ed|ée?)?\\b", 
          "\\byoga\\b", 
          "\\b(dream|ad?ventur)i?(e|è)?re?\\b", 
          "\\bliterature\\b", 
          "\\bcycli(ng|st)e?\\b", 
          "\\bvélo\\b", 
          "\\dan(c|s)eu?r?\\b", 
          "\\bentho?usiaste?\\b", 
          "\\bfilms\\b", 
          "\\blov(e(s|r)?|ing)\\b", 
          "\\breligi(ous|eu(x|se))\\b", 
          "\\bsurfeu?(r|se)\\b", 
          "\\btravel(ler)?\\b", 
          "\\bvoyage(s|eu(r|s)e?)?\\b", 
          "\\bvol(u|o)nt(ai|ee)re?\\b", 
          "\\byoung\\b", 
          "\\bgardener\\b", 
          "\\bhockey\\b", 
          "\\bdivorc(é|e)(d|e)?\\b", 
          "\\bdog\\b", 
          "\\bcat\\b", 
          "\\bhobby(ist)?\\b", 
          "\\bartiste?\\b", 
          "\\br(e|é)sidente?\\b", 
          "\\bbirder\\b", 
          "\\bknit(ter|ting)?\\b", 
          "\\bfamily\\b", 
          "\\bpeinture\\b", 
          "\\bjazz\\b", 
          "\\bcomedy\\b", 
          "\\bgolf\\b", 
          "\\b(v|w)wine?\\b", 
          "\\bcoffee\\b", 
          "\\btea\\b", 
          "\\bscuba\\b", 
          "\\bswimming\\b", 
          "\\bbik(es?|ing)\\b", 
          "\\bclimb(er|ing)\\b", 
          "\\bnerd\\b", 
          "\\bsport\\b", 
          "\\bsailing\\b", 
          "\\bhumanitarian\\b", 
          "\\bgame(s|r)\\b", 
          "\\bcamping\\b", 
          "\\bfishing\\b", 
          "\\bfencer\\b", 
          "\\bhiker\\b", 
          "\\bretired\\b", 
          "\\bbibliophile\\b", 
          "\\b(papa|maman)\\b", 
          "\\badmirer\\b", 
          "\\bjunkie\\b", 
          "\\bpassion((at|é)e?)?s?\\b", 
          "\\bbeer\\b", 
          "\\bphotography\\b", 
          "\\buncle\\b", 
          "\\btinkerer\\b", 
          "\\bplayer\\b", 
          "\\branter\\b", 
          "\\bcontemplating\\slife\\b", 
          "\\bveggies\\b", 
          "\\bski(ing|er)?\\b", 
          "\\bfoodie\\b", 
          "\\binterests\\b", 
          "\\bfreethinker\\b", 
          "\\bsurf\\b", 
          "\\bfootball\\b", 
          "\\bamateur\\b")
testpers <- c("mom", "grandma", "mother", "grandmother", "grandfather", "dad", "grandpa", "père")

grepl(paste(pers, collapse="|"),testpers)

## =================
#code to political
## =================

poli <- c("\\b#resist\\b", 
          "\\badvoca(t(e|ing)|cy)\\b", 
          "\\bactivis(m|t)e?\\b", 
          "\\bcampaign(er¡ing)\\b",
          "\\benvironmentalist\\b",
          "\\blefti(st|sh)\\b",
          "\\bnationalist\\b", 
          "\\bliberal\\b",
          "\\bconservati(sm|ve)\\b",
          "\\bmilitante?\\b", 
          "\\bfight(ing)?\\b", 
          "\\b#?climat\\s?eaction\\b", 
          "\\binequality\\b", "\\b(human|women\\s)\\srights\\b", 
          "\\bsocial\\sjustice\\b", 
          "\\b#?degrowth\\b", 
          "\\b#lowtech\\b", 
          "\\b#?progressi(ve|ste?)\\b", 
          "\\bpolicy\\b", 
          "\\b@piratepartyau\\b", 
          "\\b#berner\\b", 
          "\\#?fossil\\s?fuels?\\b", 
          "\\bpachamama\\b", 
          "\\bcouncillor\\b", 
          "\\bstand\\swith\\b",
          "\\bdecoloniz(e|ing)\\b",
          "\\btransition\\b",
          "\\bgreens\\smember\\b", 
          "\\bopposed\\b",
          "\\bgreenie\\b", 
          "\\bfeminis(m|t)\\b", 
          "\\btree\\s?hugger\\b", 
          "\\b#?divest?\\b", 
          "\\b(#go)?vegan\\b",
          "\\bbernie\\b",
          "\\bconcerned\\b",
          "\\b#?france\\s?insoumise\\b",
          "\\bemergency\\b",
          "\\balternative\\b",
          "\\bagent\\sof\\schange\\b",
          "\\bplanet\\b",
          "\\b\\w?solar\\s?warrior\\b",
          "\\b#?the\\sresistance\\b",
          "\\beco\\swarrior\\b",
          "\\bcommunity\\sleader\\b",
          "\\bdéclin\\b",
          "\\bsenator\\b",
          "\\bstop\\sclimate\\schange\\b",
          "\\b#?save\\s?the\\s?arctic\\b",
          "\\bgender\\sequity\\b",
          "\\b#earthourhome\\b",
          "\\bmayor\\b",
          "\\brefugee\\sjustice\\b",
          "\\b#?climate\\s?hawk\\b",
          "\\b#animallover\\b",
          "\\benvironmental\\sissues\\b",
          "\\b#standupforscience\\b",
          "\\bstop\\b",
          "\\bresilience\\b",
          "\\bnonuke\\b",
          "\\bcounsellor\\b",
          "\\bcalling\\sfor\\b",
          "\\bparliament\\b",
          "\\bcllr\\b",
          "\\b#renewables\\b",
          "\\bblackfish\\b",
          "\\b#?act\\s?on\\s?climate\\b",
          "\\bban\\sfracking\\b",
          "\\b#?climate\\s?justice\\b",
          "\\bresisting\\b",
          "\\bthreatened\\b",
          "\\b#feelthebern\\b",
          "\\bdeniers\\b",
          "\\b#maga\\b",
          "\\b#shutdowntanks\\b",
          "\\bclimate\\scrisis\\b",
          "\\b#?carbon\\s?neutrality\\b")
testpoli <- c("fossil fuels", "france", "solar warrior", "solar", "#solarwarrior", "#govegan", "save the arctic", "#savethearctic")

grepl(paste(poli, collapse="|"),testpoli)

## =================
#code to political
## =================

pub <-c("\\bjournals?\\b",
        "\\bpublish(er|ing)s?\\b",
        "\\b\\s?press\\b",
        "\\btaylor\\s+francis\\b",
        "\\belsevier\\b",
        "\\bwiley\\b",
        "\\broutledge\\b",
        "\\bspringer\\b",
        "\\bsage\\b",
        "\\bjrnl\\b",
        "\\bissn\\b")
testpub <- c("journal", "publishing", "publishers", "oxford press", "taylor francis", "taylor  francis")

grepl(paste(pub, collapse="|"),testpub)

## =================
#match categories to descriptions
## =================

users$Academic <- grepl(paste(sci, collapse="|"), users$description, perl=TRUE) %>%
  ifelse(1,0)
users$Communication <- grepl(paste(comm, collapse="|"), users$description, perl=TRUE) %>%
  ifelse(1,0)
users$Organization <- grepl(paste(organ, collapse="|"), users$description, perl=TRUE) %>%
  ifelse(1,0)
users$Bots <- grepl(paste(bot, collapse="|"), users$description, perl=TRUE) %>%
  ifelse(1,0)
users$Professional <- grepl(paste(pro, collapse="|"), users$description, perl=TRUE) %>%
  ifelse(1,0)
users$Personal <- grepl(paste(pers, collapse="|"), users$description, perl=TRUE) %>%
  ifelse(1,0)
users$Political <- grepl(paste(poli, collapse="|"), users$description, perl=TRUE) %>%
  ifelse(1,0)
users$Publishers <- grepl(paste(pub, collapse="|"), users$description, perl=TRUE) %>%
  ifelse(1,0)

write.csv(users, file = "Publics_Data")
