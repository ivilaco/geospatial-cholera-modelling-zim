/* ******************************************

Program name: analysis of cholera case data zim 
Author: Sophie Ayling
Date last modified: March 9th 2018
Project: Zim Cholera
Purpose:  examine borehole data

****************************************** */

***0. SET STATA 

clear matrix
clear
set more off
cap cd "C:\Users\wb488473\OneDrive - WBG\Documents - Data analysis to inform Global Water Operations - WB Group\Zim CHOLERA - via Teams\08_Data/"
// file path modified 26 feb 2023 (R&R Plos NTD)

tempfile temp1 relsis temp2 temp3 temp4 temp5

*import and sort 

use "from Dropbox\DHS 2015\DHS\ZW_2015_DHS_05082019_1112_119669\ZWHR71DT/ZWHR71FL.dta", clear

*just keep the observations for Harare and surrounding areas 

tab hv024
tab hv024,nol

keep if inlist(hv024,9,2,3,4)

order hhid hv000 hv001 hv002 hv003 hv004 hv005 hv006 hv007 hv008 hv008a hv009 hv024 hv025 hv045b hv201 hv202 hv201a hv204 hv230b hv235 hv237 hv237a hv237b hv237c hv237d hv237e hv237f hv237g hv237h hv237i hv237k hv237j hv237x hv237z hv205 hv225 hv238 hv238a

* export excel using "stata_output/DHS 2015/DHS_2015_ordered_subset.xlsx", first(varl) replace

keep hhid hv000 hv001 hv002 hv003 hv004 hv005 hv006 hv007 hv008 hv008a hv009 hv024 hv025 hv045b hv201 hv202 hv201a hv204 hv230b hv235 hv237 hv237a hv237b hv237c hv237d hv237e hv237f hv237g hv237h hv237i hv237k hv237j hv237x hv237z hv205 hv225 hv238 hv238a



label dir
numlabel `r(names)', add

tab hv201

** apply the weights

gen wt = hv005/1000000

**********ADDED CODE: 19TH APRIL 2021 - WATER RISK INDEX AND DUMMIES FOR SOURCE TYPE


gen sdg_w = .
*lowest risk is piped to premises/neighbor and bottled water
replace sdg_w = 0 if inlist(hv201, 11,12,13, 71) 

*risk 1 if piped but not continuous service 
replace sdg_w = 1 if inlist(hv201, 11,12,13, 71) & hv201a ==1

*risk 1 if public standpipe, borehole or protected well or spring within 30m walk of house
replace sdg_w = 2 if inlist(hv201, 14, 21, 31, 41) & (hv204 <= 30 | hv204 == 996)

*risk 3 if public standpipe, borehole or protected well or spring more than 30 min walk of house
replace sdg_w = 3 if inlist(hv201, 14, 21, 31, 41) & (hv204 > 30 & hv204 < 996 | hv204==998) // don't knows (998 can be coded as higher risk as more likely further distance)

*risk 4 if unprotected source - unprotected well, unprotected spring, or open water, cart with small tank
replace sdg_w = 4 if inlist(hv201, 32, 42, 43, 62 )  

*risk of 2 if non-on premise other source - rainwater, tanker truck 
replace sdg_w = 2 if inlist(hv201, 51,61 )
*risk is missing for other
replace sdg_w = . if hv201==96

*CHECK CODING
tab sdg_w, m
tab hv201 if sdg_w==0
tab hv201 if sdg_w==1
tab hv201 if sdg_w==2
tab hv201 if sdg_w==3

tab hv204 if sdg_w==0
tab hv204 if sdg_w==1
tab hv204 if sdg_w==2
tab hv204 if sdg_w==3

tab hv201 if sdg_w==2 & hv204 < 30

// okay is fine. 

*minmax normalize the index

norm sdg_w, method (mmx)
*drop sdg_w
tab sdg_w

*scenarios 1: 4--> 3 everyone gets at least far improved water  (noone has unimproved)

gen w_scenario_1 = sdg_w
replace w_scenario_1 = 3 if sdg_w ==4
tab w_scenario_1
norm w_scenario_1, method(mmx)

*scenarios 1: dummy version - noone has unimproved anymore 
gen w_unimproved_scenario1 = 0

*scenario 2:  0,1 --> 0 everyone who piped water gets continous service 

gen w_scenario_2 = sdg_w
replace w_scenario_2 = 0 if sdg_w ==1
tab w_scenario_2
norm w_scenario_2, method(mmx)

*scenario 2:  dummy version  - noone changes from sdg categories
gen w_unimproved_scenario2 = 0
replace w_unimproved_scenario2 = 1 if sdg_w == 4

*scenario 3:  0,1,2 --> 0 everyone who has non-continuous piped or improved but close gets continous piped supply 

gen w_scenario_3 = sdg_w
replace w_scenario_3 = 0 if inlist(sdg_w, 0,1,2)
tab w_scenario_3
norm w_scenario_3, method(mmx)

*scenario 3: dummy version - same as scenario 2 as noone changes from unimproved categories
gen w_unimproved_scenario3 = w_unimproved_scenario2

*scenario 4:  3,4 --> 2 everyone who has unimproved or far improved gets at least near improved 

gen w_scenario_4 = sdg_w
replace w_scenario_4 = 2 if inlist(sdg_w, 3,4)
tab w_scenario_4
norm w_scenario_4, method(mmx)

*scenario 4: dummy version - all unimproved get at least improved

gen w_unimproved_scenario4 = 0 

*************************SANITATION RISK INDEX

* sanitation risk
tab hv205 hv225,m
codebook hv205

gen sanitation_risk=.
* flush to piped sewer system & not shared
replace sanitation_risk=0 if hv205==11&hv225==0
* flush to septic tank/pit latrine / VIP latrine / pit latrine with slab & not shared
replace sanitation_risk=1 if (hv205==12|hv205==13|hv205==21|hv205==22)&hv225==0
* flush to piped sewer system & shared
replace sanitation_risk=2 if hv205==11&hv225==1
* flush to septic tank/pit latrine / VIP latrine / pit latrine with slab & shared
replace sanitation_risk=3 if (hv205==12|hv205==13|hv205==21|hv205==22)&hv225==1
* unimproved sanitation facilities
replace sanitation_risk=4 if hv205==14|hv205==15|hv205==23|hv205==42
* OD
replace sanitation_risk=5 if hv205==31

tab sanitation_risk,m

gen sdg_s = sanitation_risk
norm sdg_s, method(mmx)

***Scenario 1 of sanitation - everyone gets at least improved and unshared sanitation  5,4, 3,2--> 1  

gen s_scenario_1 = sanitation_risk 
replace s_scenario_1 = 1 if inlist(sanitation_risk, 2,3,4,5)
tab s_scenario_1
norm s_scenario_1, method(mmx)

**sewers in scenario 1 - dummy unchanged 
gen sewer_scenario1 = 0
replace sewer_scenario1= 1 if sanitation_risk== 0

***Scenario 2 of sanitation - all sewered i.e. all dummies are 1 
gen s_scenario_2 = 0
tab s_scenario_2
norm s_scenario_2, method(mmx)

**sewers in scenario 2 - 5,4,3,2,1  --> 0 (entirely sewered) dummy changed for everyone 
gen sewer_scenario2 = 0


** keep dummies of each source type

gen w_piped_to_prem=0
replace w_piped_to_prem=1 if inlist(hv201, 11,12,13)

gen w_protected_point_source = 0
replace w_protected_point_source = 1 if inlist(hv201, 14, 21, 31, 41)

gen w_unimproved_source = 0
replace w_unimproved_source = 1 if inlist(hv201, 32, 42, 43, 62 ) 

gen w_other_source = 0
replace w_other_source = 1 if inlist(hv201, 51,61 )

** ADDITIONAL CATEGORIES ON 2ND JUNE 2021

* piped water (including standpipe)
gen w_piped_source=0
replace w_piped_source=1 if inlist(hv201, 11,12,13, 14)

* protected groundwater sources (including boreholes, protected wells and springs)
gen w_protected_groundwater_source = 0
replace w_protected_groundwater_source = 1 if inlist(hv201, 21, 31, 41)

tab hv201 if w_protected_groundwater_source==1

*all other sources (mainly unprotected wells)
gen w_unprotected_and_other = 0
replace w_unprotected_and_other= 1 if inlist(hv201,32,42,43,51,61,62,71,96)

tab hv201 if w_unprotected_and_other == 1 


* 3 category one (piped source, protected groundwater source, everything else)
gen three_categories_water = 0
replace three_ = 1 if w_piped_source==1
replace three_ = 2 if w_protected_groundwater_source==1
replace three_ = 3 if w_unprotected_and_other==1

tab hv201 if three_ == 1
tab hv201 if three_ == 2
tab hv201 if three_ == 3
tab three, missing

** keep dummies for sanitation

gen sewer = 0
replace sewer = 1 if sanitation_risk== 0


*check 
tab hv201 if w_piped_to_prem==1
tab hv201 if w_piped_to_prem==0

tab hv201 if w_protected_point_source==1
tab hv201 if w_protected_point_source==0
 
tab hv201 if w_unimproved_source ==1
tab hv201 if w_unimproved_source ==0

tab hv201 if w_other_source ==1
tab hv201 if w_other_source ==0



order hv024 hv001 hhid  mmx_sdg_w w_scenario* w_* mmx_sdg_s s_scenario*  sewer* 


***** Do the collapses that Hugh wanted 
by hv001, sort: egen no_hhs = count (hhid)

collapse (first) no_hhs (sum) w_unimproved_source w_unimproved_scenario* w_piped_to_prem w_piped_source w_protected_groundwater_source w_unprotected_and_other three_categories_water sewer sewer_scenario*  (mean) sdg_* w_scenario* s_scenario* , by (hv001) 

*mmx_sdg_w mmx_sdg_s mmx_w_scenario* mmx_s_scenario* // no longer using these normalized values. Could potentially use Zee instead in future.

export excel using "stata_output/DHS 2015/DHS2015_wri_scenarios_030621.xlsx", first(varl) replace

save "stata_output/DHS 2015/DHS2015_wri_scenarios_030621.dta", replace