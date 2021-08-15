# University of Florida Advancement — Modeling Team

Walter Ogozaly, Maria Arias, Alexander Real, Madison Kaplan

### Data Description

#### UF Advancement Click Data	5.35 MB

This data is in Excel format and grouped weekly. It includes the date of any donor household’s first and last visits to our website as well as the total number of visits. Only about 20,000 households out of ~500,000 have made a recorded website visit. The model will import any spreadsheets that fit the click data’s naming convention so any new weekly files can be simply dropped in the relevant folder.

#### Demographic and Alumni data (Joined_Export_*)	732 MB

The bulk of our data, representing roughly 500,000 alum households, of which half have donated to UF. This data includes demographics (income/region/sex/married) as well as information related to alum activities and past donations. 

### Overall Findings

Our model identified 6,900 households with the highest propensity to become first-time donors to the Machen Florida Opportunity Scholarship (MFOS). A household was required to have a donation history with MFOS in order to be marked as a success case in the model’s training data, so these 6,900 households represent false positives identified by the model. Despite this, they are the desired contact group. 

In order to predict who to contact for a donation in the next month, we should note that we assumed you should contact the people likeliest to donate. While this is a straightforward assumption, you could imagine a campaign directed at medium-to-low propensity households or a campaign that sought to avoid donors thought of as “safe” or “sure”. In such a case our model’s utility would be significantly decreased.

Two time lags were developed in order to make the most informed prediction of which households would donate over the next 31 days. One of these lags, formulated by looking at autocorrelation of donations since 2000, was 1 year—i.e., the most predictive interval between donations is 1 year, so we ought to look at April ‘20 donors when predicting April ‘21 donors. The other lag was any individual donor’s average MFOS donation interval. The model’s task was to identify the households that met either lag condition, with the caveat of not having access to ANY contact or donation variables. Forcing the model to rely on donation-independent variables prevented it from eliminating first-time donors on the basis of their first-time-ness, which would have run counter to the business need. False positives are valuable in our model output because we sought households who resembled the predicted upcoming donors in all ways except having a gift history with MFOS.


#### Code Handover


Our code is available on Github (https://github.com/walter97/UFF_Models), but since the modeling was done entirely on a virtual machine the sponsor also has easy access to it via a shared folder.

#### Model Versions
##### MODEL 1.1 	First logistic regression for madeDonationLastMonth
- Included independent variables weeksSinceLastDonation, weeksSinceFirstDonation, and GIFT_COUNT. 

##### MODEL 1.2 	Logistic regression for variable reduction
- This was the first model to include all variables intended for the final product.
- In conjunction with a confusion matrix and our Model 1.3 random forest importance chart, this logistic regression was used to identify variables for elimination.
- Built after Model 1.3, but logically a precursor. 

##### MODEL 1.3 	Regression, Decision Tree, and RM on madeDonationLastMonth
- First models to use SMOTE, k-fold cross validation, and centering/scaling of data.
- High correlation between variables diminished these models’ effectiveness.
- RM model still yielded valuable results for variable reduction in Model 1.2.

##### MODEL 2.1	Regression, Decision Tree, and RM on likelyToDonateThisMonth
- The random forest ran on only half the training data (split on the dependent variable) due to machine limitations. This was still ~180k observations.
- The newly developed dependent variable likelyToDonateThisMonth was the largest update to these models. These results reflect real people who should be reached out to instead of just those who donated very recently.
- All three models yielded significant results with high accuracy.  

##### FINAL MODEL	Decision tree predicting likelyToDonateThisMonth
- This model had the highest accuracy of predicting likelyToDonateThisMonth, accompanied by the highest TP and lowest FN rates.
