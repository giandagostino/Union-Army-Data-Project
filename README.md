# Union Army Dataset
Determining long-term effects of Tuberculosis (TB) Pre-Treatment using Union Army Soldiers
* What are the survival rates for TB-affected soldiers compared to other soldiers?
* What are the differences in enlistment and hospitilization timelines between the two cohorts? 

#### Dataset Description
<table>
  <thead>
    <tr>
      <th>Dataset</th>
      <th>Description</th>
      <th>Analytical Role</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>mil</td>
      <td>Regimental, Pension, Compiled Military Service, Carded Medical Records</td>
      <td>Demographics, hospitalization records, baseline covariates</td>
    </tr>
    <tr>
      <td>dis</td>
      <td>Surgeon's Certificates</td>
      <td>Disease onset and medical event timing</td>
    </tr>
    <tr>
      <td>msr</td>
      <td>Military Service Records</td>
      <td>Service duration</td>
    </tr>
    <tr>
      <td>cen</td>
      <td>1850, 1860, 1900, 1910 Federal Census Records</td>
      <td>Birth/Death dates, socioeconomic/geographic Background</td>
    </tr>
  </tbody>  
</table>


This dataset is particularly useful because the entire observation period predates effective tuberculosis treatment, eliminating this as a time-varying confounder. As a result, differences in survival can be interpreted as reflecting underlying demographic, environmental, and service-related factors rather than changes in treatment access or efficacy.

I compiled all of these records into a single dataset (`all`) by each recruit's unique `recidnum`

## Cleaning and Cohorts
The 39,339 soldiers were separated into into two cohorts, `tb` and `ntb`. Recruits were put in the `tb` bucket if, anywhere in their records, there was any mention of the substrings `"tuber"`,
`"consump"`, or `"phth"`. Tuberculosis, consumption, and phthisis, were all common ways of referring to TB during this time.

<table>

  <thead>
    <tr>
      <th>Cohort</th>
      <th>Number of Soldiers</th>
    </tr>
  </thead>

  <tbody>
    <tr>
      <td>tb</td>
      <td>2,560</td>
    </tr>
    <tr>
      <td>ntb</td>
      <td>36,778</td>
    </tr>
  </tbody>
</table>

To resolve inconsistencies in birth and death years (of which there were many), the following methodology was used, prioritizing the most reliable sources at the top:
* 1900 Census
* 1910 Census
* 1860 Census
* 1850 Census
* Physical Exams (required for pension application)
* Military Recruitment Records

## Analytical Methods

### Survival Analysis
TB-affected soliders with reliable

## Results

## Limitations



