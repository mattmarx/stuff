set more off
**VERSION HISTORY
*081 surely I meant to add a comment to this file when I changed it!
*080 a bunch of info on the inventors and assignees
*079 drop self cites instead of preserving them.
*078 adding dependent variables for academic non-self cites, corporate non-self cites. incorporate paper-level heterogeneity, which twin is first
*077 now requires input of radius. must be a way to default this but can't think of it!
*076 create positive-hub versions of interactions; add twin heterogeneity. also drivetime
*075 save the matched hubs not just 0/1 indicator
*074 add # of inventors near paper
*073 add counts of patents in subclass within past 5 years
*072 add conference counts
*071 prestige not just max of the fields, but the field of the paper (oops)
*070 don't log jif;create separate ljif variable. set = 0 if missing. calculate hubvariation, cityprestige_field
*069 undo settung jiff and stocckpat/pub to 0 if missing
*068 assume jif = 0 if missing
*067 by-field prestige 
*066 non-upcase variable names
*063-065 adding different distance dummies, also isoXdistance dummies
*062 get patents-to-date for org not just past 5 years
*061 now collecting both autho overlap and local-patent overlap
*060 add counts of patents per year /institution where we didn't have from TTO
*059 replace 'tto' with 'ttoalready'; assume zero values for TTO variables if missing
*058
*057 new TTO variables
*056 new prestige variables
*055 shared coinventors
*054 distributed teams
*053
*052
*051 do not drop the twins without patents
*048 sort by patent twinid ref_ID to ensure same ordering in onlypatentdata
*047 merge with ppp_d
*046 unknown
*045 moved the basic stuff to twinssetup001.do so we can "kick" the list of patents when needed (had commented it out).
*044 fix bug where patent clusters are taken same year as paper clusters
*043 switch to doing paper/patisolated programmatically in a loop 0 1 5 not separately
*042 DO NOT USE
*041 add support for patisolated5, move isolated5 to paperisolated5. no change to 1 or 0
*040 switched from doing clusters at the top/3-digit classification level to the subclass. also see changes in nonisolated005
*039 fixed bug where Michael's list of false-hit citations were NOT removed from the analysis set
*038 fixed bug where we were assuming every non-cited article was isolated

syntax anything

args simdistarg

global simdist "`simdistarg'"
di "***** simdist =  $simdist"

label define matchl 0 "none" 1 "loose" 2 "tight"

clear all

*************TWINSMATCH START

use twinsmatch, clear
sort patent
merge m:1 patent using patentsfortwins, keep(1 3) nogen
drop descrip
sort patcity patstate patcountry 
merge m:1 patent using asglocuniq, keep(1 3) nogen
replace patcity = asgcity if missing(patcity)
replace patcity = trim(proper(patcity))
replace patstate = asgstate if missing(patstate) & patcountry=="USA"
drop asgcity asgstate
* look up latlong for patent locations
rename patcity city
rename patstate state
rename patzip zip
rename patcountry country
sort city state zip country
merge m:1 city state zip country using citystatezipcountrylatlong, keep(1 3) nogen
rename latitude pat_latitude
rename longitude pat_longitude
sort city state country
merge m:1 city state country using citystatecountrylatlong, keep(1 3) nogen
replace pat_latitude = latitude if missing(pat_latitude) & ~missing(latitude)
replace pat_longitude = longitude if missing(pat_longitude) & ~missing(longitude)
drop latitude longitude
rename city patcity
rename country patcountry
rename state patstate
rename zip patzip 
* repeat for pub locations (could loop)
rename papercity city
rename paperstate state
rename paperzip zip
rename papercountry country
sort city state zip country
merge m:1 city state zip country using citystatezipcountrylatlong, keep(1 3) nogen
rename latitude paperlatitude
rename longitude paperlongitude
sort city state country
merge m:1 city state country using citystatecountrylatlong, keep(1 3) nogen
replace paperlatitude = latitude if missing(paperlatitude) & ~missing(latitude)
replace paperlongitude = longitude if missing(paperlongitude) & ~missing(longitude)
drop latitude longitude
rename city papercity
rename country papercountry
rename state paperstate
rename zip paperzip 
* not sure why doesn't match
replace paperlatitude = 7.989737 if papercountry=="Cote d'Ivoire"
replace paperlongitude = -5.56795 if papercountry=="Cote d'Ivoire"

/*
foreach x of varlist *latitude *longitude {
  //destring `x', replace force
  replace `x' = `x' / 57.29577951 if ~missing(`x')
  }
*/
  * MSA assignment
gen city = papercity
gen state = paperstate
//replace city  = proper(trim(city))
//replace state = proper(trim(state))
sort city state
replace city = "Saint Louis" if city=="St. Louis" | city=="St Louis"
replace city = "Durham" if city=="Research Triangle Park"
replace city = "Saint Paul" if city=="St. Paul"
replace city = "Winston Salem" if city=="Winston-Salem"
replace city = "Champaign" if city=="Urbana-Champaign"
replace city = "Houston" if city=="Woodlands"
replace city = "Cambridge" if regexm(city, "Massachusetts Institute")
merge m:1 city state using citystatezip, keep(1 3)
replace paperzip = zip if missing(paperzip) & ~missing(zip)
replace paperzip = zip if city=="San Diego" & state=="CA"
drop city state zip
drop _merge
sort patent
sort paperzip
rename paperzip zip
merge m:1 zip using zipmsa, keep(1 3) nogen
merge m:1 zip using zipcbsa, keep(1 3) nogen
rename zip paperzip
rename cbsa papercbsa
sort msa
rename msa papermsa
replace papermsa = pubmsa if missing(papermsa)
rename papermsa msa
merge m:1 msa using msacbsa, keep(1 3) nogen
replace papercbsa = cbsa if missing(papercbsa)
rename msa papermsa
drop cbsa
drop pubmsa
tostring patzip, replace
replace patzip = "0" + patzip if length(patzip)==4
rename patzip zip
merge m:1 zip using zipmsa, keep(1 3) nogen
merge m:1 zip using zipcbsa, keep(1 3) nogen
rename zip patzip
rename msa patmsa
rename cbsa patcbsa
rename patmsa msa
merge m:1 msa using msacbsa, keep(1 3) nogen
replace patcbsa = cbsa if missing(patcbsa)
rename msa patmsa
drop cbsa
compress
merge m:1  patent using patacad, keep(1 3) nogen
merge m:1 ref_ID using alltwinids
drop if _merge==2
count if _merge==1 // these are ref_IDs where neither it nor its twin had a cite
//drop if _merge==1
drop _merge
* now set up the joinby
** fisrt make a list of all twins for reach patent (build a strong, so we can regexm later)
* finally, merge in the sponsor data & tag where patent assignee = sponsor
merge m:1 ref_ID using sponsors, keep(1 3) nogen
merge m:1 patent using patmultasg, keep(1 3) nogen
* for each assignee (single or multiple), check if it is a subset of the sponsors
* clean & lowercase all before comparing

* GET THE CLUSTERS FOR EACH DYAD'S SUBCLASS, MATCHING THE PAPER'S YEAR
* FOR ALL YEARS (0) YOU CAN JUST COPY
merge m:1 classprimsub using topclasspatlocs0, keep(1 3) nogen
replace numclusters0 = 0 if missing(numclusters0)
rename numclusters0 papernumclusters0
rename classprimsubclusters0 paperclassprimsubclusters0
gen patnumclusters0 = papernumclusters0
gen patclassprimsubclusters0 = paperclassprimsubclusters0
* FOR 1 AND 5 YEARS, YOU NEED TO DO A SEPARATE MERGE FOR PATENTS USING A DIFFERENT YEAR(RANGE)
* 1 first
* DO FOR PAPERS
merge m:1 classprimsub year using topclasspatlocs1, keep(1 3) nogen
replace numclusters1 = 0 if missing(numclusters1)
rename numclusters1 papernumclusters1
rename classprimsubclusters1 paperclassprimsubclusters1
* DO FOR PATENTS
rename year oldyear
tostring appyearstr, gen(year)
merge m:1 classprimsub year using topclasspatlocs1, keep(1 3) nogen
replace numclusters1 = 0 if missing(numclusters1)
rename numclusters1 patnumclusters1
rename classprimsubclusters1 patclassprimsubclusters1
drop year
rename oldyear year
* now 5
merge m:1 classprimsub yearrange using topclasspatlocs5, keep(1 3) nogen
replace numclusters5 = 0 if missing(numclusters5)
rename numclusters5 papernumclusters5
rename classprimsubclusters5 paperclassprimsubclusters5
gen patyearrange = ""
replace patyearrange = "1903-1979" if appyearstr<1980
replace patyearrange = "1980-1984" if appyearstr>1979 & appyearstr<1985
replace patyearrange = "1985-1989" if appyearstr>1984 & appyearstr<1990
replace patyearrange = "1990-1994" if appyearstr>1989 & appyearstr<1995
replace patyearrange = "1995-1999" if appyearstr>1994 & appyearstr<2000
replace patyearrange = "2000-2004" if appyearstr>1999 & appyearstr<2005
replace patyearrange = "2005-2010" if appyearstr>2004
rename yearrange paperyearrange
rename patyearrange yearrange
merge m:1 classprimsub yearrange using topclasspatlocs5, keep(1 3) nogen
replace numclusters5 = 0 if missing(numclusters5)
rename numclusters5 patnumclusters5
rename classprimsubclusters5 patclassprimsubclusters5
rename yearrange patyearrange
rename paperyearrange yearrange
*when we're done bysort ref_ID to see if any where not isolated.
* NEED TO RENAME ALL OF THE MERGED-IN VARIABLES: classprimsub, yearrange, numclusters5, classprimsubclusters5
* NOW DO THE SAME THING BY THE PATENT'S YEAR SO THAT YOU GET THE RELEVANT CLUSTERS
* rename patent year variable so merge will work
* create year-range variable for patent
save twinsmatchpatdetails, replace
/*
use twinsmatchpatdetails, clear
*/

** then separate into only patent and only paper data
use twinsmatchpatdetails, clear
drop numcites patcbsa numasgs patent patcity patstate patcountry patzip appyearstr fwcit fwcit5  pat_latitude pat_longitude  patmsa  pat_acad intpatent strpatent assignee binventors inventors asg1 asg2 asg3 asg4 primclass subclass classprimsub *numclusters* *classprimsubclusters*
duplicates drop ref_ID, force
save onlypaperdata, replace

use twinsmatchpatdetails, clear
drop if missing(patent)
drop authors correspond firstauthor  papercbsa  corp sponsors year yearrange volume page papercity paperstate papercountry paperzip title journal paperlatitude paperlongitude papermsa
gen refs4patent = ""
sort patent twinid ref_ID
replace refs4patent = "|" + ref_ID + "|" if patent~=patent[_n-1]
replace refs4patent = refs4patent[_n-1] + ref_ID + "|" if patent==patent[_n-1]
drop ref_ID
gen refs4patentlen = length(refs4patent)
bysort patent: egen maxrefs4patentlen = max(refs4patentlen)
gen revrefs4patentlen = maxrefs4patentlen - refs4patentlen
sort patent revrefs4patentlen
replace refs4patent = refs4patent[_n-1] if patent==patent[_n-1]
drop *refs4patentlen 
duplicates drop patent twinid, force
save onlypatentdata, replace

** joinby on the twin id & set flag for actual cite
use onlypaperdata, clear
joinby twinid using onlypatentdata, unmatched(master)
gen uncitedsimdis = _merge==1
drop _merge
tostring twinid, gen(twinidstr)
gen twinpat = twinidstr + patent
drop twinidstr
gen actualcite = 0
replace actualcite = 1 if regexm(refs4patent, ref_ID)
drop refs4patent
** do all comparisons, including self-cites
destring year, gen(paperyear)
gen lag = appyearstr - paperyear
replace lag = 0 if lag<0
gen nlnamematches = 0
gen lnamematches = ""
forvalues i = 1(1)22 {
 di "binventor #`i': " word(binventors,`i')
 qui replace nlnamematches = nlnamematches + 1 if ~missing(word(binventors, `i')) & regexm(authors, " " + word(binventors, `i') + "[ ,\.;]")
 qui replace lnamematches = lnamematches + "|" + word(binventors, `i') if ~missing(word(binventors, `i')) & regexm(authors, " " + word(binventors, `i') + "[ ,\.;]")
}

gen nlnamefinitmatches = 0
gen lnamefinitmatches = ""
gen copyinventors = inventors
gen head = ""
gen tail = ""
forvalues i = 1(1)22 {
  qui replace head = regexs(1) if regexm(copyinventors, "^([A-Za-z -]+)$")
  qui replace tail = "" if regexm(copyinventors, "^([A-Za-z -]+)$")
  qui replace head = regexs(1) if regexm(copyinventors, "^([A-Za-z -]+)\|(.*)")
  qui replace tail = regexs(2) if regexm(copyinventors, "^([A-Za-z -]+)\|(.*)") 
  qui di "head: " head " tail: " tail
  qui replace nlnamefinitmatches = nlnamefinitmatches + 1 if ~missing(trim(head)) & regexm(authors, head + ".")
  qui replace lnamefinitmatches = lnamefinitmatches + "|" + trim(head) if ~missing(trim(head)) & regexm(authors, head + ".")
  qui replace copyinventors = tail
  qui replace head = ""
  qui replace tail = ""
}
drop copyinventors head tail
gen selfpersoncite = nlnamefinitmatches>0 | nlnamematches>1
gen selfpersonciteflex = selfpersoncite==1 | length(lnamematches)>4
gen selforgcite = 0
sort ref_ID patent
merge m:1 ref_ID patent using michaelselforgcites, keep(1 3)
replace selforgcite = 1 if _merge==3
drop _merge
sort ref_ID patent
merge m:1 ref_ID patent using mattbyhandselforgcites, keep(1 3)
replace selforgcite = 1 if _merge==3
drop _merge
sort ref_ID patent
//merge m:1 ref_ID patent using maybenotselforgcites, keep(1 3)
//replace selforgcite = 0 if _merge==3
//drop _merge
gen selforgciteauto = 0
gen smallassignee = assignee
replace smallassignee = regexr(assignee, "the regents of the ", "")
replace selforgciteauto = 1 if selforgcite==0 & ~missing(trim(smallassignee)) & regexm(lower(correspond), smallassignee)
replace selforgcite = 1 if selforgciteauto==1
drop  strpatent intpatent numcites
gen selfcite = selfpersoncite==1 | selforgcite==1
gen nonselfcite = 0
replace nonselfcite = 1 if actualcite==1
replace nonselfcite = 0 if selfcite==1
gen nonselfcorpcite = actualcite==1 & selfcite==0 & pat_acad==0
gen nonselfacadcite = actualcite==1 & selfcite==0 & pat_acad==1
bysort ref_ID: egen tempnumnonselfcites = nvals(patent) if selfcite==0
replace tempnumnonselfcites = 0 if missing(tempnumnonselfcites)
bysort ref_ID: egen numnonselfcites = max(tempnumnonselfcites)
gen selfcitex = selfpersonciteflex==1| selforgcite==1
gen samecountry = 0
replace samecountry = 1 if ~missing(papercountry) & ~missing(patcountry) & lower(trim(papercountry))==lower(trim(patcountry)) 
replace samecountry = 0 if ~missing(papercountry) & ~missing(patcountry) & lower(trim(papercountry))~=lower(trim(patcountry))
gen samestate = 0
replace samestate = 1 if samecountry==1 & ~missing(paperstate) & ~missing(patstate) & lower(trim(paperstate))==lower(trim(patstate))
replace samestate = 0 if ~missing(paperstate) & ~missing(patstate) & lower(trim(paperstate))~=lower(trim(patstate))
replace samestate = . if (papercountry~="USA" & papercountry~="Canada") | (patcountry~="USA" & patcountry~="Canada")
gen samemsa = 0
replace samemsa = 1 if samecountry==1 & ~missing(papermsa) & ~missing(patmsa) & lower(trim(papermsa))==lower(trim(patmsa))
replace samemsa = 0 if ~missing(papermsa) & ~missing(patmsa) & lower(trim(papermsa))~=lower(trim(patmsa))
replace samemsa = . if papercountry~="USA" | patcountry~="USA"
gen samecbsa = 0
replace samecbsa = 1 if samecountry==1 & ~missing(papercbsa) & ~missing(patcbsa) & lower(trim(papercbsa))==lower(trim(patcbsa))
replace samecbsa = 0 if ~missing(papercbsa) & ~missing(patcbsa) & lower(trim(papercbsa))~=lower(trim(patcbsa))
replace samecbsa = . if papercountry~="USA" | patcountry~="USA"
gen samecity = 0
replace samecity = 1 if samecountry==1 & ~missing(papercity) & ~missing(patcity) & lower(trim(papercity))==lower(trim(patcity))
replace samecity = 0 if ~missing(papercity) & ~missing(patcity) & lower(trim(papercity))~=lower(trim(patcity))
vincenty paperlatitude paperlongitude pat_latitude pat_longitude, loc(distmi)
replace distmi = 0 if missing(distmi) & samecity==1
/*
gen distkm = .
replace distkm = 3963.1 * acos((sin(paperlatitude)*sin(pat_latitude)) + (cos(paperlatitude)*cos(pat_latitude)*cos(paperlongitude - pat_longitude)))
replace distkm = 0 if paperlatitude==pat_latitude & paperlongitude==pat_longitude 
replace distkm = 0 if distkm<1
replace distkm = . if missing(paperlatitude) | missing(paperlongitude) | missing(pat_latitude) | missing(pat_longitude)
gen distmi = .
replace distmi = distkm/(1.6) if ~missing(distkm)
*/
gen distkm = distmi*1.6 if ~missing(distmi)
gen ldistkm = .
replace ldistkm = log(1 + distkm) if ~missing(distkm)
gen ldistmi = .
replace ldistmi = log(1 + distmi) if ~missing(distmi)
drop binventors inventors nlname* lname*
gen samelanguage = 0
gen lcountry = ""
replace lcountry = papercountry
sort lcountry
merge m:1 lcountry using countrylangs, keep(1 3) nogen
rename langs paperlangs
replace lcountry = patcountry
merge m:1 lcountry using countrylangs, keep(1 3) nogen
rename langs patlangs
drop lcountry
gen copypaperlangs = paperlangs
gen head = ""
gen tail = ""
forvalues i = 1(1)4 {
  qui replace head = regexs(1) if regexm(copypaperlangs, "^([A-Za-z -]+)$")
  qui replace tail = "" if regexm(copypaperlangs, "^([A-Za-z -]+)$")
  qui replace head = regexs(1) if regexm(copypaperlangs, "^([A-Za-z -]+)\|(.*)")
  qui replace tail = regexs(2) if regexm(copypaperlangs, "^([A-Za-z -]+)\|(.*)") 
   di copypaperlangs "head: " head " tail: " tail
  replace samelanguage = 1 if ~missing(trim(head)) & regexm(patlangs, head)
  qui replace copypaperlangs = tail
  qui replace head = ""
  qui replace tail = ""
}
foreach x in sponsors assignee asg1 asg2 asg3 asg4 {
 qui replace `x' = subinstr(`x', ".", "",.)
 qui replace `x' = subinstr(`x', "-", " ",.)
 qui replace `x' = subinstr(`x', "/", " ",.)
 qui replace `x' = subinstr(`x', "'", " ",.)
 qui replace `x' = subinstr(`x', "\'", " ",.)
 qui replace `x' = subinstr(`x', ",", "",.)
 qui replace `x' = subinstr(`x', "(", "",.)
 qui replace `x' = subinstr(`x', ")", "",.)
 qui replace `x' = subinstr(`x', "  ", " ",.)
 qui replace `x' = trim(lower(`x'))
 qui replace `x' = "hoffmann la roche" if `x'=="hoffman la roche"
 qui replace `x' = "hoffmann la roche" if `x'=="hoffmann la roche inc"
 qui replace `x' = "hoffmann la roche" if `x'=="hoffmann la roche company"
 qui replace `x' = "bristol meyers squibb" if `x'=="bristol myers squibb company"
 qui replace `x' = "bristol meyers squibb" if `x'=="bristol myers squibb"
 qui replace `x' = "amgen" if `x'=="amgen canada inc"
}
gen asgsponsoredpat = 0
 replace asgsponsoredpat = 1 if regexm(sponsors, assignee) & ~missing(assignee) & ~missing(sponsors)
 replace asgsponsoredpat = 1 if regexm(sponsors, asg1) & ~missing(asg1) & ~missing(sponsors)
 replace asgsponsoredpat = 1 if regexm(sponsors, asg2) & ~missing(asg2) & ~missing(sponsors)
 replace asgsponsoredpat = 1 if regexm(sponsors, asg3) & ~missing(asg3) & ~missing(sponsors)
 replace asgsponsoredpat = 1 if regexm(sponsors, asg4) & ~missing(asg4) & ~missing(sponsors)

drop copypaperlangs head tail
gen bothinUS = papercountry=="USA" & patcountry=="USA"
gen bothnonUS = papercountry~="USA" & patcountry~="USA"
gen dist0 = distmi==0
gen dist0to10 = distmi>0 & distmi<=10 & ~missing(distmi)
gen dist10to20 = distmi>10 & distmi<=20 & ~missing(distmi)
gen dist20to30 = distmi>20 & distmi<=30 & ~missing(distmi)
gen dist30to40 = distmi>30 & distmi<=40 & ~missing(distmi)
gen dist40to50 = distmi>40 & distmi<=50 & ~missing(distmi)
gen dist50to75 = distmi>50 & distmi<=75 & ~missing(distmi)
gen dist75to100 = distmi>75 & distmi<=100 & ~missing(distmi)
gen dist100to150 = distmi>100 & distmi<=150 & ~missing(distmi)
gen dist150to200 = distmi>150 & distmi<=200 & ~missing(distmi)
gen dist200to300 = distmi>200 & distmi<=300 & ~missing(distmi)
gen dist300to400 = distmi>300 & distmi<=400 & ~missing(distmi)
gen dist400to500 = distmi>400 & distmi<=500 & ~missing(distmi)
gen dist500to750 = distmi>500 & distmi<=750 & ~missing(distmi)
gen dist750to1000 = distmi>750 & distmi<=1000 & ~missing(distmi)
gen dist1000to1500 = distmi>1000 & distmi<=1500 & ~missing(distmi)
gen dist1500to2000 = distmi>1500 & distmi<=2000 & ~missing(distmi)
gen dist2000to2500 = distmi>2000 & distmi<=2500 & ~missing(distmi)
gen dist2500to4000 = distmi>2500 & distmi<=4000 & ~missing(distmi)
gen dist4000to6000 = distmi>4000 & distmi<=6000 & ~missing(distmi)
gen dist6000plus = distmi>6000 & ~missing(distmi)
gen dist0to20 = distmi>=0 & distmi<20
gen dist20to50 = distmi>=20 & distmi<50
gen dist50to200 = distmi>=50 & distmi<200
gen dist50to250 = distmi>=50 & distmi<250
gen dist200to1000 = distmi>=200 & distmi<1000
gen dist1000to2500 = distmi>=1000 & distmi<2500
gen distover2500 = distmi>2500
gen dist20to250 = distmi>=20 & distmi<250
gen dist250to1000 = distmi>=250 & distmi<1000

gen contigcountries = 0
gen country1 = ""
gen country2 = ""
replace country1 = papercountry
replace country2 = patcountry
sort country1 country2
merge m:1 country1 country2 using contigcountries, keep(1 3)
replace contigcountries = 1 if _merge==3
drop _merge
replace country1 = patcountry
replace country2 = papercountry
sort country1 country2 
merge m:1 country1 country2 using contigcountries, keep(1 3)
replace contigcountries = 1 if _merge==3
drop _merge
gen contigstates = .
replace contigstates = 0 if (papercountry=="USA" & patcountry=="USA")
gen state1 = ""
gen state2 = ""
replace state1 = paperstate
replace state2 = patstate
sort state1 state2
//merge m:1 state1 state2 using ../knowdiff/neighboring_states, keep(1 3)
//replace contigstates = 1 if _merge==3
//drop _merge
replace state1 = patstate
replace state2 = paperstate
sort state1 state2
//merge m:1 state1 state2 using ../knowdiff/neighboring_states, keep(1 3)
//replace contigstates = 1 if _merge==3
//drop _merge
* wrap it up
bysort twinid: egen twincommoncite = mean(actualcite) if selfcite==0
bysort ref_ID: egen refcited = mean(actualcite) if selfcite==0
gen papercity5pct = 0
replace papercity5pct = 1 if papercity=="Boston" | papercity=="San Diego" || papercity=="San Francisco" || papercity=="Cambridge" 
gen paperCA = paperstate=="CA"
gen paperMA = paperstate=="MA"
gen paperus = papercountry=="USA"
sort ref_ID
merge m:1 ref_ID using refacad30122014, keep(1 3) nogen
rename CA_acad paperacad
gen paperyrpre90 = paperyear<1991
gen paperyr9094 = paperyear>1989 & paperyear<1995
gen paperyr9599 = paperyear>1994 & paperyear<2000
gen paperyr0004 = paperyear>1999 & paperyear<2005
gen paperyr0509 = paperyear>2004

merge m:1 ref_ID using noncorresplocs, keep(1 3) nogen

gen patlatitude = pat_latitude
gen patlongitude = pat_longitude
foreach j in /*0 1*/ 5 {
 foreach k in paper /*pat*/ {
 di "BUILDING  `k'  `j'"
  qui gen `k'dyadmatchedhubs`j' = ""
  capture drop head tail copyclassprimsubclusters`j' 
  capture drop *nearclassprimsubcluster`j' 
  gen head = ""
  gen tail = ""
  gen copyclassprimsubclusters`j' = `k'classprimsubclusters`j' 
  gen `k'nearclassprimsubcluster`j' = 0
  gen `k'noncorrespinhub`j' = 0
  gen `k'noncorrespnearhub`j' = 0
   forvalues i = 1(1)1350 { // hardcoded max # of cluster cities for any primary/subclass
   if (`i'>`k'numclusters`j') {
    continue
   }
  qui gen tempcountry = ""
  qui gen tempcity = ""
  qui gen tempstate = ""
  qui gen templat = ""
  qui gen templong = ""
  qui gen temppaperdistmi = .
  qui gen temppatdistmi = .
 * the usual walk through; mind the semicolons
   qui replace head = regexs(1) if regexm(copyclassprimsubclusters`j', "^;([A-Za-z0-9,\. -]+);$")
   qui replace tail = "" if regexm(copyclassprimsubclusters`j', "^;([A-Za-z0-9,\. -]+);$")
   qui replace head = regexs(1) if regexm(copyclassprimsubclusters`j', "^;([A-Za-z0-9,\. -]+);(;.*;)")
   qui replace tail = regexs(2) if regexm(copyclassprimsubclusters`j', "^;([A-Za-z0-9,\. -]+);(;.*;)") 
   //di "head: " head 
   //di "tail: " tail
   foreach x in country city state lat long {
    qui replace temp`x' = ""
   }
   * pick apart pices of head
   qui replace tempcountry = regexs(1) if regexm(head, "^([A-Za-z -\.]+),([A-Za-z]*),([A-Za-z\. -]+),(-?[0-9]*\.[0-9]*),(-?[0-9]*\.[0-9]*)$")
   qui replace tempstate = regexs(2) if regexm(head, "^([A-Za-z -\.]+),([A-Za-z]*),([A-Za-z\. -]+),(-?[0-9]*\.[0-9]*),(-?[0-9]*\.[0-9]*)$")
   qui replace tempcity = regexs(3) if regexm(head, "^([A-Za-z -\.]+),([A-Za-z]*),([A-Za-z\. -]+),(-?[0-9]*\.[0-9]*),(-?[0-9]*\.[0-9]*)$")
   qui replace templat = regexs(4) if regexm(head, "^([A-Za-z -\.]+),([A-Za-z]*),([A-Za-z\. -]+),(-?[0-9]*\.[0-9]*),(-?[0-9]*\.[0-9]*)$")
   qui replace templong = regexs(5) if regexm(head, "^([A-Za-z -\.]+),([A-Za-z]*),([A-Za-z\. -]+),(-?[0-9]*\.[0-9]*),(-?[0-9]*\.[0-9]*)$")
   qui destring templat, replace
   qui destring templong, replace
	* DO THE VERY SIMPLE IS NON-CORRESPONDING AUTHOR IN ONE OF THE HUBS. PROBLEM IS THAT SOMETIMES YOU GET DIFFERENT LATLONGS WHICH DON'T MATCH.
   replace `k'noncorrespinhub`j' = 1 if regexm(noncorresplocs, head) & ~missing(head) & ~missing(noncorresplocs)
   //di "************************************************<hub>" head "<noncor>" noncorresplocs //if regexm(noncorresplocs, head)
   * CYCLE THROUGH THE NON-CORRESPONDING AUTHORS, DO THE DISTANCE CHECK
   //di "NOW CHECKING FOR NON-CORRESPONDING AUTHORS NEAR HUBS"
   qui capture drop copynoncorresplocs
   gen copynoncorresplocs = noncorresplocs
   qui capture drop tempnchead tempnctail
   qui gen tempnchead = ""
   qui gen tempnctail = ""
   forvalues m = 1(1)21 {
    if (`m'>numnoncorresp) {
	 continue
	}
	//di "NONCORRESPLOCS " copynoncorresplocs
	qui gen tempnccountry = ""
	qui gen tempnccity = ""
	qui gen tempncstate = ""
	qui gen tempnclat = ""
	qui gen tempnclong = ""
	qui gen tempncdistmi = ""
	qui replace tempnchead = regexs(1) if regexm(copynoncorresplocs, "^;([A-Za-z0-9,\. -]+);$")
	qui replace tempnctail = "" if regexm(copynoncorresplocs, "^;([A-Za-z0-9,\. -]+);$")
	qui replace tempnchead = regexs(1) if regexm(copynoncorresplocs, "^;([A-Za-z0-9,\. -]+);(;.*;)")
	qui replace tempnctail = regexs(2) if regexm(copynoncorresplocs, "^;([A-Za-z0-9,\. -]+);(;.*;)")
	//di "HEAD " tempnchead
	//di "TAIL " tempnctail
	qui replace tempnccountry = regexs(1) if regexm(tempnchead, "^([A-Za-z -\.]+),([A-Za-z]*),([A-Za-z\. -]+),(-?[0-9]*\.[0-9]*),(-?[0-9]*\.[0-9]*)$")
    qui replace tempncstate = regexs(2) if regexm(tempnchead, "^([A-Za-z -\.]+),([A-Za-z]*),([A-Za-z\. -]+),(-?[0-9]*\.[0-9]*),(-?[0-9]*\.[0-9]*)$")
    qui replace tempnccity = regexs(3) if regexm(tempnchead, "^([A-Za-z -\.]+),([A-Za-z]*),([A-Za-z\. -]+),(-?[0-9]*\.[0-9]*),(-?[0-9]*\.[0-9]*)$")
    qui replace tempnclat = regexs(4) if regexm(tempnchead, "^([A-Za-z -\.]+),([A-Za-z]*),([A-Za-z\. -]+),(-?[0-9]*\.[0-9]*),(-?[0-9]*\.[0-9]*)$")
    qui replace tempnclong = regexs(5) if regexm(tempnchead, "^([A-Za-z -\.]+),([A-Za-z]*),([A-Za-z\. -]+),(-?[0-9]*\.[0-9]*),(-?[0-9]*\.[0-9]*)$")
    //di "COMPARING " tempcountry "=" tempnccountry " / " tempstate "=" tempncstate " / " tempcity "=" tempnccity
	replace `k'noncorrespnearhub`j' = 1 if ~missing(`k'classprimsubclusters`j') & ~missing(noncorresploc) & tempcountry==tempnccountry & tempstate==tempncstate & tempcity==tempnccity
	replace `k'dyadmatchedhubs`j' = `k'dyadmatchedhubs`j' + /*tempcountry + "," + tempstate + "," + tempcity + "," +*/ tempnclat + "," + tempnclong + "|" if ~regexm(`k'dyadmatchedhubs`j', tempcountry + "," + tempstate + "," + tempcity) & ~regexm(`k'dyadmatchedhubs`j', tempnclat + "," + tempnclong) & ~missing(noncorresploc) & tempcountry==tempnccountry & tempstate==tempncstate & tempcity==tempnccity
	qui destring tempnclat, replace 
	qui destring tempnclong, replace 
	qui vincenty templat templong tempnclat tempnclong, loc(tempnc`k'distmi) replace
	//di "VINCENTY " templat " " templong " " tempnclat " " tempnclong " difference was " tempnc`k'distmi
	replace `k'noncorrespnearhub`j' = 1 if  !missing(templat) & !missing(templong) & !missing(tempnclat) & !missing(tempnclong) & tempnc`k'distmi<$simdist
	qui tostring tempnclat, replace
	qui tostring tempnclong, replace
	replace `k'dyadmatchedhubs`j' = `k'dyadmatchedhubs`j' + /*tempcountry + "," + tempstate + "," + tempcity + "," +*/ tempnclat + "," + tempnclong + "|" if ~regexm(`k'dyadmatchedhubs`j', tempcountry + "," + tempstate + "," + tempcity) &  ~regexm(`k'dyadmatchedhubs`j', tempnclat + "," + tempnclong) & !missing(templat) & !missing(templong) & !missing(tempnclat) & !missing(tempnclong) & tempnc`k'distmi<$simdist
	qui replace copynoncorresplocs = tempnctail
	qui replace tempnchead = ""
	qui replace tempnctail = ""
	qui drop tempnccountry tempnccity tempncstate tempnclat tempnclong tempnc*distmi 
   }
   //l tempcountry papercountry 
   //l tempcity papercity
   //l tempstate paperstate
   //l templat paperlatitude
   //l templong paperlongitude
   * set nearness = 1 if same country city state
   qui replace `k'nearclassprimsubcluster`j' = 1 if `k'country==tempcountry & `k'state==tempstate & `k'city==tempcity
   qui tostring templat, replace
   qui tostring templong, replace
      qui replace `k'dyadmatchedhubs`j' = `k'dyadmatchedhubs`j' + /*tempcountry + "," + tempstate + "," + tempcity + "," +*/ templat + "," + templong + "|" if ~regexm(`k'dyadmatchedhubs`j', tempcountry + "," + tempstate + "," + tempcity) & ~regexm(`k'dyadmatchedhubs`j', templat + "," + templong) & `k'country==tempcountry & `k'state==tempstate & `k'city==tempcity
   * if has lat/long, compute distance from paper and set nearness variable = 1 if within 20 miles?
   qui destring templat, replace
   qui destring templong, replace
   qui vincenty `k'latitude `k'longitude templat templong, loc(temp`k'distmi) replace
   //l tempdistmi 
   qui tostring templat, replace
   qui tostring templong, replace
   qui replace `k'nearclassprimsubcluster`j' = 1 if ~missing(templat) & ~missing(templong) & temp`k'distmi<$simdist
   qui replace `k'dyadmatchedhubs`j' = `k'dyadmatchedhubs`j' + /*tempcountry + "," + tempstate + "," + tempcity + "," +*/ templat + "," + templong + "|" if ~regexm(`k'dyadmatchedhubs`j', tempcountry + "," + tempstate + "," + tempcity) & ~regexm(`k'dyadmatchedhubs`j', templat + "," + templong) & ~missing(templat) & ~missing(templong) & temp`k'distmi<$simdist
   qui destring templat, replace
   qui destring templong, replace

   qui replace copyclassprimsubclusters`j' = tail
   qui replace head = ""
   qui replace tail = ""
   qui drop tempcountry tempcity tempstate templat templong  temp*distmi 
  }
  qui drop copyclassprimsubclusters`j' head tail
  bysort twinid ref_ID: egen temppapernotisolated`j' = max(`k'nearclassprimsubcluster`j')
  * make sure every entry has some hub in it, not all will be retained
  /*gen matchedhubslen = length(`k'dyadmatchedhubs`j')
  bysort twinid ref_ID: egen maxmatchedhubslen = max(matchedhubslen)
  //gen revmatchedhubslen = maxmatchedhubslen - matchedhubslen
  gsort twinid ref_ID  -matchedhubslen // revmatchedhubslen
  bysort twinid ref_ID: replace `k'dyadmatchedhubs`j' = `k'dyadmatchedhubs`j'[_n-1] + "|" + `k'dyadmatchedhubs`j'
  by twinid ref_ID: replace `k'dyadmatchedhubs`j' = `k'dyadmatchedhubs`j'[_N]*/
  //bysort twinid patent: egen temppatnotisolated`j' = max(`k'nearclassprimsubcluster`j')
  qui gen `k'notisolated`j' = .
  qui replace `k'notisolated = temp`k'notisolated`j' 
  qui drop temppa*notisolated*
  qui gen `k'isolated`j' = ~`k'notisolated`j'
  drop `k'notisolated`j'
 }
 //gen paperpatbothisolated`j' = paperisolated`j' * patisolated`j'
}
 
gen distauthpaperisolated5 = 0
replace distauthpaperisolated5 = 1 if paperisolated5==1
replace distauthpaperisolated5 = 0 if papernoncorrespnearhub5==1
gen nearhub = 1 - paperisolated5
gen distauthnearhub = 1 - distauthpaperisolated5


bysort twinid: gen numrefs4twin = _N
bysort twinid: egen numnonhubrefs4twin = sum(distauthpaperisolated5)
gen numhubrefs4twin = numrefs4twin - numnonhubrefs4twin
gen twinhubvariation  = numnonhubrefs4twin>0 & numhubrefs4twin>0
drop *hubrefs4twin
 
capture drop *prestige org
merge m:1 ref_ID using twinorgchars, keep(1 3) nogen
replace prestige = 0 if missing(prestige)
foreach x of varlist prestige_* {
 replace `x' = 0 if missing(`x')
}


bysort ref_ID: egen numcites = sum(actualcite)
bysort ref_ID: egen numnonselfacadcites = sum(nonselfacadcite)
bysort ref_ID: egen numnonselfcorpcites = sum(nonselfcorpcite)
bysort papercity: egen numincity = nvals(ref_ID)
bysort org: egen numatorg = nvals(ref_ID)

//JIF
gen titleabbrev = ""
replace titleabbrev = regexs(1) if regexm(ref_ID, "[0-9], (.*)")
replace titleabbrev = regexs(1) if regexm(ref_ID, "[0-9], (.*)[\']")
replace titleabbrev = regexs(1) if regexm(ref_ID, "[0-9], (.*)[\'][\']")
merge m:1 titleabbrev using jif20095, keep(1 3)
drop _merge
replace jif = 0 if missing(jif)
gen ljif = log(1 + jif)

//TTO
gen na = papercountry=="USA" | papercountry=="Canada"
merge m:1 org year using ttodata, keep(1 3)
gen ttohasannualdata = _merge==3
drop _merge
foreach x in ttoalready ttoexp ttopats ttofte ttolicfte ttoothfte {
 gen `x'miss = `x'
 replace `x' = 0 if missing(`x')
}
* TTOEXP is in millions, so cut it down to size
replace ttoexp = ttoexp / 1000000
merge m:1 org year using nonttopatsperyear, keep(1 3) nogen
replace orgnumpatsperyear = ttopats if missing(orgnumpatsperyear)
replace orgnumpatsperyear = 0 if missing(orgnumpatsperyear)
gen lorgnumpatsperyear = log(1+ orgnumpatsperyear)
replace orgnumpatspast5years = ttopatspast5years if missing(orgnumpatspast5years)
replace orgnumpatspast5years = 0 if missing(orgnumpatspast5years)
gen lorgnumpatspast5years = log(1+ orgnumpatspast5years)
replace orgnumpats2date = ttopats2date if missing(orgnumpats2date)
gen lorgnumpats2date = log(1 + orgnumpats2date)

bysort twinid: egen tempnumuspapersfortwin = nvals(ref_ID) if paperus==1
replace tempnumuspapersfortwin = 0 if missing(tempnumuspapersfortwin)
bysort twinid: egen numuspapersfortwin = max(tempnumuspapersfortwin)
drop tempnumuspapersfortwin
gen keeptwinfortto = numuspapersfortwin>1
drop numuspapersfortwin
gen jif2 = jif^2
//done
    gen dist10to50 = dist10to20 | dist20to30 | dist30to40 | dist40to50
    //gen dist50to200 = dist50to75 | dist75to100 | dist100to150 | dist150to200  
    //gen dist200to1000 = dist200to300 | dist300to400 | dist400to500 | dist500to750 | dist750to1000
    gen dist1000to6000 = dist1000to1500 | dist1500to2000 | dist2000to2500 | dist2500to4000 | dist4000to6000
    drop dist10to20 dist20to30 dist30to40 dist40to50 dist50to75 dist75to100 dist100to150 dist150to200 dist200to300 dist300to400 dist400to500 dist500to750 dist750to1000 dist1000to1500 dist1500to2000 dist2000to2500 dist2500to4000 dist4000to6000
replace jif2 = jif^2
merge m:1 ref_ID using PatentPaperPairs, keep(1 3) nogen // note that 550 uncited articles show up here
compress

gen acadhasnselfindref = 0
replace acadhasnselfindref = 1 if paperacad==1 & pat_acad==0 & selfcite==0 & actualcite==1
bysort twinid: egen twinacadhasnselfindref = max(acadhasnselfindref)

merge m:1 ref_ID using indiv_level_datav13, keep(1 3) nogen
replace experience = 0 if experience<0 // buggy data
rename ref_ID ref_id
merge m:1 ref_id using refid_tc_1246_0622, keep(1 3) nogen
rename ref_id ref_ID
rename tc papercitestopaper
gen lpapercitestopaper = log(1 + papercitestopaper)
gen lpapercitestopaper2 = log(1 + papercitestopaper^2)
merge m:1 twinid ref_ID using ref_IDissue, nogen keep(1 3)
foreach x in twinsamejournal twinsameissue twinsback2back firstb2btwin {
 replace `x' = 0 if missing(`x')
}
rename assignee lcassignee
rename smallassignee assignee
capture drop _merge
merge m:1 patent using patsperpatassignee, keep(1 3) nogen
gen twinpatid=string(twinid)+"||"+patent
gen patus = patcountry=="USA"
gen dyadus = paperus==1 & patus==1
* MICHAEL SAYS THIS IS NOT A REAL TWIN
//drop if twinid==437

gen isoXdist = paperisolated5 * ldistmi
gen isoXppp = paperisolated5 * ppp_d
gen under10mi = samecity | dist0to10
drop dist0to10
gen isoXunder10mi = paperisolated5 * under10mi
* NEW PRESTIGE MEASURES
gen prestige_field = 0
merge m:1 ref_ID using paperfield, keep(1 3) nogen
foreach x in biochem biotech tissue cellbio chemistry genetics immunology materials medicinegen medicineexp nanotech oncology physicsapp physicsmat {
 replace prestige_field = prestige_`x' if field_`x'==1 & prestige_`x' > prestige_field
}
//foreach x of varlist prestige_* {
// replace prestige_field = `x' if `x'>prestige_field & !missing(`x') // take the highest of its possible fields
//}
//gen lprestige_field = log(1 + prestige_field)
foreach x in prestige prestige_field op_best op_all op_noamb op_nonet {
 gen l`x' = log( 1 + `x')
 gen l`x'2 = l`x'^2
 summ l`x',d
 gen l`x'_q1 = l`x'<=r(p25)
 gen l`x'_q2 = l`x'>r(p25) & l`x'<=r(p50)
 gen l`x'_h1 = l`x'<=r(p50)
 gen l`x'_h2 = l`x'>=r(p50)
 gen l`x'_q3 = l`x'>r(p50) & l`x'<=r(p75)
 gen l`x'_q4 = l`x'>=r(p75)
 gen isoXl`x' = paperisolated5 * l`x'
 gen hubXl`x' = distauthnearhub * l`x'
 foreach y in h1 h2 q1 q2 q3 q4 {
  gen isoXl`x'_`y' = paperisolated5 * l`x'_`y'
  gen hubXl`x'_`y' = distauthnearhub * l`x'_`y'
 } 
}
merge m:1 papercity paperstate papercountry using cityprestige, keep(1 3) nogen
replace cityprestige = 0 if missing(cityprestige)
replace lcityprestige = 0 if missing(lcityprestige)
gen cityprestige_field = 0
* calc city prestige (for tht field)
foreach x of varlist cityprestige_* {
 replace cityprestige_field = `x' if `x'>cityprestige_field & ~missing(`x') // take the highest of its possible fields
}
gen lcitypathhi = log(1 + citypathhi)

merge m:1 classprimsub papercountry paperstate papercity using locnumtop15papers, keep(1 3) nogen
replace locnumtop15papers = 0 if missing(locnumtop15papers)
replace llocnumtop15papers = 0 if missing(llocnumtop15papers)
merge m:1 papercountry paperstate papercity using twincitypop, keep(1 3) nogen
replace pop = 0 if missing(pop)
replace lpop = 0 if missing(lpop)
/*merge 1:1 ref_ID patent using paperauthcoinvoverlap, keep(1 3) nogen
merge 1:1 ref_ID patent using paperpatcoinvoverlap, keep(1 3) nogen
foreach x in auth localpat {
 gen isoXany`x'overlap = paperisolated5 * any`x'overlap
 gen l`x'overlap = log(1 + `x'overlap)
 gen isoX`x'overlap = paperisolated5 * `x'overlap
 gen isoXl`x'overlap = paperisolated5 * l`x'overlap
}
*/

gen notlowprestige = 1 - lprestige_q1
gen prestige2plus = prestige>=2
gen isoXnotlowprestige = paperisolated5 * notlowprestige
gen isoXprestige2plus = paperisolated5 * prestige2plus
foreach x of varlist ttoexp* tto*fte* {
 gen l`x' = log(1 + `x')
 gen isoX`x' = paperisolated5 * `x'
 gen isoXl`x' = paperisolated5 * l`x'
 gen hubX`x' = distauthnearhub * `x'
 gen hubXl`x' = distauthnearhub * l`x'
}
gen isoXttoalready = paperisolated5 * ttoalready
destring year, gen(intyear)
replace stockpat=0 if missing(stockpat)
replace stockpub=0 if missing(stockpub)
foreach x in jif stockpat distmi stockpub experience orgnumpatspast5years orgnumpats2date prestige ttoexp lag {
 //replace `x' = 0 if missing(`x')
 capture gen `x'2 = `x'^2
 capture gen l`x' = log(1 + `x')
 capture gen l`x'2 = l`x'^2
}

gen isoXdist0to20 = paperisolated5 * dist0to20
gen isoXdist20to50 = paperisolated5 * dist20to50
gen isoXdist50to200 = paperisolated5 * dist50to200
gen isoXdist50to250 = paperisolated5 * dist50to250
gen isoXdist200to1000 = paperisolated5 * dist200to1000
gen isoXdist1000to2500 = paperisolated5 * dist1000to2500
gen isoXdistover2500 = paperisolated5 * distover2500
gen isoXdist20to250 = paperisolated5 * dist20to250
gen isoXdist250to1000 = paperisolated5 * dist250to1000

* GENERATE INDUSTRY-INVESTMENT RANGES SO YOU CAN PLOT THEM
gen ix__0to0 = ttoexp==0
//gen ix__over0to2 = ttoexp>0 & ttoexp<2
//gen ix__0to2 = ttoexp>=0 & ttoexp<2
gen ix__over0to5 = ttoexp>0 & ttoexp<5
gen ix__5to22 = ttoexp>=5 & ttoexp<22
//gen ix__10to20 = ttoexp>=10 & ttoexp<20
//gen ix__15to20 = ttoexp>=15 & ttoexp<20
//gen ix__15to22 = ttoexp>=15 & ttoexp<22
gen ix__omitted = ttoexp>=22
foreach x of varlist ix__* {
 gen isoX`x' = paperisolated5 * `x'
 gen hubX`x' = distauthnearhub * `x'
}
 ** DO PRESTIGE RANGES SO YOU CAN PLOT IT
 gen op_nonet0__10 = op_nonet>=0 & op_nonet<10
 gen op_nonet10__40 = op_nonet>=10 & op_nonet<40
 gen op_nonet40__125 = op_nonet>=40 & op_nonet<125
 gen op_nonetomitted = op_nonet>=125
 foreach x of varlist op_nonet*__* op_nonetomitted {
  gen isoX`x' = paperisolated5 * `x'
 }

drop samestate
gen samestate = 0
replace samestate = 1 if paperstate==patstate & ~missing(paperstate) & ~missing(patstate)
do biotechclusters001
gen notbiotechcluster = 1 - biotechcluster

foreach x in 0to0 over0to5 5to22 omitted {
 gen disoXix__`x' = distauthpaperisolated5 * ix__`x'
 gen dhubXix__`x' = distauthnearhub * ix__`x'
}
foreach x in 0__10 10__40 40__125 omitted {
 gen disoXop_nonet`x' = distauthpaperisolated5 * op_nonet`x'
}
foreach x in 0to20 20to50 50to250 250to1000 1000to2500 over2500 {
 gen disoXdist`x' = distauthpaperisolated5 * dist`x'
 gen hubXdist`x' = distauthnearhub * dist`x'
}


forvalues i = 0(1)10 {
 gen lag`i'y = lag==`i'
 gen isoXlag`i'y = distauthpaperisolated5 * lag`i'y
 gen hubXlag`i'y = distauthnearhub * lag`i'y
}

* merge in info regarding conferences
gen nconfs = 0
merge m:1 papercity paperstate papercountry using paperlocconfs, keep(1 3) nogen
foreach x in biochem biotech tissue cellbio chemistry genetics immunology materials medicinegen medicineexp nanotech oncology physicsapp physicsmat {
 replace nconfs_`x' = 0 if missing(nconfs_`x')
 replace nconfs = nconfs_`x' if field_`x'==1 & nconfs_`x' > nconfs
}
gen anyconfs = nconfs>0
sort papercity paperstate papercountry
summ nconfs if ~(papercity==papercity[_n-1] & paperstate==paperstate[_n-1] & papercountry==papercountry[_n-1]),d
gen nconfs_abvmd = nconfs>r(p50)
gen nconfs_abvmn = nconfs>r(mean)
gen nconfs_abv75 = nconfs>r(p75)
gen nconfs_abv90 = nconfs>r(p90)
gen nconfs_abv95 = nconfs>r(p95)
gen nconfs_abv99 = nconfs>r(p99)
replace nconfs_all = 0 if missing(nconfs_all)
gen lnconfs = log(1 + nconfs)
gen lnconfs_all = log(1 + nconfs_all)
gen confhub = distauthnearhub==1 & nconfs_abvmn==1
gen nonconfhub = distauthnearhub==1 & nconfs_abvmn==0

* merge in counts of  nearby patents
merge m:1 papercity paperstate papercountry yearrange classprimsub using subclasspatsnearpapercity5, keep(1 3) nogen
foreach x in 10 25 50 75 100 250 500 1000 2500 5000 10000 25000 {
 replace patswithin`x'm = 0 if missing(patswithin`x'm)
 sum patswithin`x'm,d
 foreach y in 50 75 90 95 99 {
  gen patswithin`x'mp`y' = patswithin`x'm > r(p`y')
 }
 gen lpatswithin`x'm = log(1 + patswithin`x'm)
 replace pctpatswithin`x'm = 0 if missing(pctpatswithin`x'm)
 gen lpctpatswithin`x'm = log(1 + pctpatswithin`x'm)
 gen pctpatswithin`x'm_min = pctpatswithin`x'm
 replace pctpatswithin`x'm_min = . if numclassprimsubpats5<5
 gen lpctpatswithin`x'm_min = log(1 + pctpatswithin`x'm_min)
 replace lpctpatswithin`x'm_min = . if numclassprimsubpats5<5
}

* merge in count of inventors on paper, inventor experience
merge m:1 patent using invexp, keep(1 3) nogen
foreach x in numinvs avginvexp totinvexp medinvexp {
 replace `x' = 1 if missing(`x')
 gen l`x' = log(1 + `x')
}

* merge in whether patent was in life sciences
merge m:1 primclass using lifesciclasses, keep(1 3) nogen
replace lifesciclass = 0 if missing(lifesciclass)
rename lifesciclass lifescipat

 * mergein countsof inventors near paper
merge 1:1 patent ref_ID using previnvnearpaper, keep(1 3) nogen
replace numinvsnearpaper = 0 if missing(numinvsnearpaper)
gen invprevnearpaper = numinvsnearpaper>0

* merge in counts of inventors near hub
merge 1:1 twinid patent ref_ID using previnvnearhub, keep(1 3) nogen
replace numinvsnearhub = 0 if missing(numinvsnearhub)
gen lnuminvsnearhub = log(1 + numinvsnearhub)
gen anyinvnearhub = numinvsnearhub>0

* merge in measures of twin heterogeneity
merge m:1 twinid using twinshet, keep(1 3) nogen
* do not replace missing values! these are defined only for a subset where postdocs inspected the papers
merge m:1 ref_ID using twinpapershet, keep(1 3) nogen

 
merge m:1 ref_ID patent using drivetime, keep(1 3) nogen
gen drivetime = durationseconds / 60 / 60 / 24
gen drivetimemiss = drivetime
replace drivetimemiss = 3 if missing(drivetimemiss)

merge m:1 ref_ID using jaccard, keep(1 3) nogen
drop jac*
replace thistwinfirst = 0 if missing(thistwinfirst)
 
merge m:1 ref_ID patent using refpatcoauthoroverlap, keep(1 3) nogen

replace lnumpatspast5yrs = 0 if missing(lnumpatspast5yrs)

 capture gen dist0to20 = distmi<21
 capture gen dist21to2000 = distmi>20 & distmi<2001
 capture gen dist2001plus = distmi>2000
 capture gen hubX0to20 = distauthnearhub * dist0to20
 capture gen hubX21to2000 = distauthnearhub * dist21to2000
 capture gen hubX2001plus = distauthnearhub * dist2001plus

label var coauthorpatoverlap "Coauthor of paper author is inventor on patent"
label var nconfs "# conferences in this field held within 50 miles"
label var lnconfs "# conferences in this field held within 50 miles"
label var twinpaper_moredetail "This paper more detailed than its twin(s)"
label var twinpaper_moreclaims "This paper has more claims than its twin(s)"
label var twinpaper_clearer "This paper written more clearly than its twin(s)"
label var twinpaper_clinical "This paper more clinical than its twin(s)"
label var thistwinfirst "This paper published before its twin(s)"
label var confhub "Paper authors near a hub of relevant R&D & >median # of conferences within 50m"
label var nonconfhub "Paper authors not near a hub of relevant R&D or <median # of conferences within 50m"
label var actualcite "Twin paper referenced by focal patent"
label var paperisolated5 "Paper corresponding author outside hub of relevant R&D"
label var distauthnearhub "Paper authors near a hub of relevant R&D"
label var distauthpaperisolated5 "Paper authors outside hubs of relevant R&D"
label var notbiotechcluster "Paper outside biotech clusters"
label var jif "Journal impact factor"
label var ljif "Journal impact factor"
label var paperus "Paper located in U.S."
label var ppp_d "Paper was patented"
label var stockpat "Corresponding author stock of patents"
label var lstockpat "Corresponding author stock of patents"
label var stockpub "Corresponding author stock of papers"
label var lstockpub "Corresponding author stock of papers"
label var experience "Corresponding author years since Ph.D"
label var lexperience "Corresponding author years since Ph.D"
label var univ "Institution is a university"
label var orgnumpatspast5years "Institution's 5-year stock of patents"
label var lorgnumpatspast5years "Institution's 5-year stock of patents"
label var prestige "Institutional prestige"
label var prestige_field "Institutional prestige in paper's field"
label var lprestige "Institutional prestige"
label var lag "Publication lag, paper vs. patent"
label var llag "Publication lag, paper vs. patent"
label var distmi "Distance between paper and patent"
label var ldistmi "Distance between paper and patent"
label var dist0to20 "Paper and patent <20 miles apart"
label var dist21to2000 "Paper and patent 20-2000 miles apart"
label var dist2001plus "Paper and patent 2000+ miles apart"
label var dist20to50 "Paper and patent 20-50 miles apart"
label var dist50to250 "Paper and patent 50-250 miles apart"
label var dist250to1000 "Paper and patent 250-1000 miles apart"
label var dist1000to2500 "Paper and patent 1000-2500 miles apart"
label var isoXdist0to20 "Outside hubs, within 20 miles of patent"
label var isoXdist20to50 "Outside hubs, 20-50 miles from patent"
label var isoXdist50to250 "Outside hubs, 50-250 miles from patent"
label var isoXdist250to1000 "Outside hubs, 250-1000 miles from patent"
label var isoXdist1000to2500 "Outside hubs, 1000-2500 miles from patent"
label var hubXdist0to20 "In a hub & within 20 miles of patent"
label var hubX0to20 "In a hub & within 20 miles of patent"
label var hubX21to2000 "In a hub & within 20-2000 miles of patent"
label var hubX2001plus "In a hub & 2000+ miles away from patent"
label var hubXdist20to50 "In a hub & 20-50 miles from patent"
label var hubXdist50to250 "In a hub & 50-250 miles from patent"
label var hubXdist250to1000 "In a hub & 250-1000 miles from patent"
label var hubXdist1000to2500 "In a hub & 1000-2500 miles from patent"
label var hubXdistover2500 "In a hub & more than 2500 miles from patent"
label var disoXdist0to20 "Outside hubs, within 20 miles of patent"
label var disoXdist20to50 "Outside hubs, 20-50 miles from patent"
label var disoXdist50to250 "Outside hubs, 50-250 miles from patent"
label var disoXdist250to1000 "Outside hubs, 250-1000 miles from patent"
label var disoXdist1000to2500 "Outside hubs, 1000-2500 miles from patent"
label var disoXdistover2500 "Outside hubs, more than 2500 miles from patent"
label var samecountry "Paper and patent in same country"
label var samestate "Paper and patent in same state"
label var ttoalready "Institution has technology transfer office"
label var ttoexp "Industry $ funding research at institution"
label var lttoexp "Industry $ funding research at institution"
label var isoXttoalready "Outside hubs, institution has technology transfer office"
label var isoXlttoexp "Outside hubs * industry $ funding institution"
label var isoXttoexp "Outside hubs * industry $ funding institution"
label var op_nonet0__10  "Institutional prestige: lowest quartile"
label var op_nonet10__40  "Institutional prestige: second-lowest quartile"
label var op_nonet40__125  "Institutional prestige: second-highest quartile"
label var op_nonetomitted  "Institutional prestige: highest quartile"
label var isoXop_nonet0__10  "Outside hubs, lowest quartile prestige"
label var isoXop_nonet10__40  "Outside hubs, second-lowest quartile prestige"
label var isoXop_nonet40__125  "Outside hubs, second-highest quartile prestige"
label var isoXop_nonetomitted  "Outside hubs, highest quartile prestige"
label var disoXop_nonet0__10  "Outside hubs, lowest quartile prestige"
label var disoXop_nonet10__40  "Outside hubs, second-lowest quartile prestige"
label var disoXop_nonet40__125  "Outside hubs, second-highest quartile prestige"
label var disoXop_nonetomitted  "Outside hubs, highest quartile prestige"
label var ix__0to0  "Institution has no industry funding"
label var ix__over0to5  "Institution has little industry funding"
label var ix__5to22 "Institution has more industry funding"
label var ix__omitted  "Institution has most industry funding"
label var isoXix__0  "Outside hubs, no industry funding"
label var isoXix__over0to5  "Outside hubs, little industry funding"
label var isoXix__5to22 "Outside hubs, more industry funding"
label var isoXix__omitted  "Outside hubs, most industry funding"
label var hubXix__0  "In a hub & no industry funding"
label var hubXix__over0to5  "In a hub & little industry funding"
label var hubXix__5to22 "In a hub & more industry funding"
label var hubXix__omitted  "In a hub & most industry funding"
label var disoXix__0  "Outside hubs, no industry funding"
label var disoXix__over0to5  "Outside hubs, little industry funding"
label var disoXix__5to22 "Outside hubs, more industry funding"
label var disoXix__omitted  "Outside hubs, most industry funding"
label var hubXttoexp "Near a hub * industry $ funding research at institution"
label var lprestige_field_h1 "Institutional presetige in paper's field below median"
label var hubXlprestige_field_h1 "In a hub & prestige below mean"
label var lprestige_field_h2 "Institutional presetige in paper's field above mean"
label var hubXlprestige_field_h2 "In a hub & prestige above mean"
label var patswithin50m "Patents in this field within 50m"
label var pctpatswithin50m "% of patents in this field within 50m"
label var drivetime "Driving time between paper and patent"
label var drivetimemiss "Driving time between paper and patent, large for missing values"
label var lnuminvs "Ln number of inventors on the patent"
label var anyinvnearhub "Patent inventor is near one of paper's hubs" 
label var lifescipat "Life-sciences patent"
label var lavginvexp "Average experience of patent inventors"
label var lnumpatspast5yrs "Assignee's patents in past 5 years"
label var patus "Lead inventor in U.S."

* for LPM
capture encode twinpatid, gen(twinpatid_c)

di "start total"
unique ref_ID
drop if paperacad==0
di "after dropping industry papers"
unique ref_ID
bysort twinid patent: egen twinpatnumref_IDs = nvals(ref_ID)
drop if twinpatnumref_IDs==1
drop twinpatnumref_IDs
di "after dropping twins relying on industry papers" 
unique ref_ID
drop if selfcite==1
di "after dropping twins with only self-citations" 
unique ref_ID

* VERY IMPORTANT DO NOT DELETE THIS SORT!!!
sort *
format twinid %5.0g
save regreadytwins, replace


count
unique twinid
unique ref_ID
unique patent
summ actualcite









* data missingness tests
count if missing(papercountry) 
count if missing(patcountry) 
count if missing(papercity) 
count if missing(patcity) 
count if missing(patstate)  & patcountry=="US"
count if missing(paperstate)  & papercountry=="US"
count if missing(patmsa)  & patcountry=="US"
count if missing(papermsa)  & papercountry=="US"
count if missing(distkm) 
count if missing(paperlatitude) 
count if missing(pat_latitude) 
