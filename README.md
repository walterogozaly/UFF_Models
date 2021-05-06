UF Foundation Modeling Team

Walter Ogozaly	Maria Arias		Alexander Real	Madison Kaplan

Data Description
UF Advancement Click Data	5.35 MB
This data is in Excel format and grouped weekly. It includes the date of any donor household’s first and last visits to our website as well as the total number of visits. Only about 20,000 households out of ~500,000 have made a recorded website visit. The model will import any spreadsheets that fit the click data’s naming convention so any new weekly files can be simply dropped in the relevant folder.

Demographic and Alumni data (Joined_Export_*)	732 MB

	The bulk of our data, representing roughly 500,000 alum households, of which half have donated to UF. This data includes demographics (income/region/sex/married) as well as information related to alum activities and past donations. 

Overall Findings
	Our model identified 6,900 households with the highest propensity to become first-time donors to the Machen Florida Opportunity Scholarship (MFOS). A household was required to have a donation history with MFOS in order to be marked as a success case in the model’s training data, so these 6,900 households represent false positives identified by the model. Despite this, they are the desired contact group. 
	In order to predict who to contact for a donation in the next month, we should note that we assumed you should contact the people likeliest to donate. While this is a straightforward assumption, you could imagine a campaign directed at medium-to-low propensity households or a campaign that sought to avoid donors thought of as “safe” or “sure”. In such a case our model’s utility would be significantly decreased.
	Two time lags were developed in order to make the most informed prediction of which households would donate over the next 31 days. One of these lags, formulated by looking at autocorrelation of donations since 2000, was 1 year—i.e., the most predictive interval between donations is 1 year, so we ought to look at April ‘20 donors when predicting April ‘21 donors. The other lag was any individual donor’s average MFOS donation interval. The model’s task was to identify the households that met either lag condition, with the caveat of not having access to ANY contact or donation variables. Forcing the model to rely on donation-independent variables prevented it from eliminating first-time donors on the basis of their first-time-ness, which would have run counter to the business need. False positives are valuable in our model output because we sought households who resembled the predicted upcoming donors in all ways except having a gift history with MFOS.


Code Handover
Our code is available on Github (https://github.com/walter97/UFF_Models), but since the modeling was done entirely on a virtual machine the sponsor also has easy access to it via a shared folder.