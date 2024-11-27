# Sleep Apnea Cost

**Authors:** Jani Miettinen, Ida Alakörkkö, Anna But & Reijo Sund.
**Version:** 1.0 (2022-11-28).

Sleep Apnea Cost calculator is an application which estimates cost of
sleep apnea by the prevalences of medical conditions connected to sleep
apnea. This project has received funding from the European Union’s
Horizon 2020 research and innovation programme under grant agreement no.
965417. This document describes data and methods used in sleep apnea top
to down calculation. More detailed playbook of the project can be found
at <https://research.janimiettinen.fi/material/sleep22/>.

![](img/alllogos.png) <img src="img/uef.png" style="width:4.0%" />

## Data Sources

Sleep apnea prevalences, condition prevalences and population sizes are
collected from different open data sources and from article tables. Data
sources are and collected variables are:

1.  Global Health Data Exchange, IHME Data <http://ghdx.healthdata.org/>
    -   population by age/gender
    -   condition prevalences and mortalities
2.  [Benjafield et al. (2019) *Estimation of the global prevalence and
    burden of obstructive sleep apnoea: a literature-based
    analysis*](https://pubmed.ncbi.nlm.nih.gov/31300334/)
    -   OSA rates for different countries (30-69 years old population)
3.  Borsoi L, Armeni P, Donin G, Costa F, and Ferini-Strambi L. (2021)
    *The invisible costs of obstructive sleep apnea (OSA): a
    cost-of-illness analysis,* PLoS ONE, pp. 1–23, 2021, doi:
    10.1371/journal.pone.0268677. & [Armeni et al. (2019)
    *Cost-of-illness study of Obstructive Sleep Apnea Syndrome (OSAS) in
    Italy*](https://cergas.unibocconi.eu/sites/default/files/files/Cost-of-illness-study-of-Obstructive-Sleep-Apnea-Syndrome-%2528OSAS%2529-in-Italy_Report%25281%2529.pdf)
    -   Condition Prevalences, OR and RR rates, annual costs
    -   OSA rates
4.  EuroStat <https://ec.europa.eu/eurostat>
    -   Money index correction (2019) for 45 countries

## Cost Calculation

Costs are dynamically calculated in Sleep Apnea Calculator and in Map
Visualization cost are pre-calculated. There is small difference on cost
( = tens of euros) between application, which is caused by decimals and
big population sizes. More detailed version of how the costs are
calculated can be followed in [the project data
playbook](https://research.janimiettinen.fi/material/sleep22/) and from
the articles [The Economic Cost Of Obstructive Sleep Apnea - A
Systematic Review](http://urn.fi/urn:nbn:fi:uef-20220894) and
[Cost-of-illness study of Obstructive Sleep Apnea Syndrome (OSAS) in
Italy](https://cergas.unibocconi.eu/sites/default/files/files/Cost-of-illness-study-of-Obstructive-Sleep-Apnea-Syndrome-%2528OSAS%2529-in-Italy_Report%25281%2529.pdf).

### Conditions and Population

Populations are filtered from IHME data to 15-74 years old population in
2019 for each country.

We use condition prevalences presented in Armeni et al. article as base
values for each country (*Italy values*). If condition is not listed in
IHME data, base values are used. Following condition are matched in IHME
dataset:

<table>
<colgroup>
<col style="width: 23%" />
<col style="width: 8%" />
<col style="width: 10%" />
<col style="width: 46%" />
<col style="width: 11%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">condition</th>
<th style="text-align: center;">prevalence</th>
<th style="text-align: center;">ihme_cause_id</th>
<th style="text-align: center;">ihme_cause_name</th>
<th style="text-align: center;">ihme_prevalence</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">All-cause mortality</td>
<td style="text-align: center;">0.0030</td>
<td style="text-align: center;">294</td>
<td style="text-align: center;">All causes, mortality</td>
<td style="text-align: center;">0.0034775</td>
</tr>
<tr class="even">
<td style="text-align: left;">Cardiovascular mortality</td>
<td style="text-align: center;">0.0010</td>
<td style="text-align: center;">1023</td>
<td style="text-align: center;">Other cardiovascular and circulatory
diseases, mortality</td>
<td style="text-align: center;">0.0000171</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Cancer Overall</td>
<td style="text-align: center;">0.0420</td>
<td style="text-align: center;">1029</td>
<td style="text-align: center;">Total cancers</td>
<td style="text-align: center;">0.0466369</td>
</tr>
<tr class="even">
<td style="text-align: left;">Diabetic retinopathy</td>
<td style="text-align: center;">0.0260</td>
<td style="text-align: center;">669</td>
<td style="text-align: center;">Sense organ diseases</td>
<td style="text-align: center;">0.2099629</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Diabetic kidney disease</td>
<td style="text-align: center;">0.0150</td>
<td style="text-align: center;">998</td>
<td style="text-align: center;">Chronic kidney disease due to diabetes
mellitus type 2</td>
<td style="text-align: center;">0.0155526</td>
</tr>
<tr class="even">
<td style="text-align: left;">Type 2 diabetes</td>
<td style="text-align: center;">0.0680</td>
<td style="text-align: center;">976</td>
<td style="text-align: center;">Diabetes mellitus type 2</td>
<td style="text-align: center;">0.1023615</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Metabolic syndrome</td>
<td style="text-align: center;">0.3300</td>
<td style="text-align: center;">619</td>
<td style="text-align: center;">Endocrine, metabolic, blood, and immune
disorders</td>
<td style="text-align: center;">0.1600425</td>
</tr>
<tr class="even">
<td style="text-align: left;">Erectile dysfunction</td>
<td style="text-align: center;">0.1000</td>
<td style="text-align: center;">594</td>
<td style="text-align: center;">Urinary diseases and male
infertility</td>
<td style="text-align: center;">0.1361221</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Stroke</td>
<td style="text-align: center;">0.0020</td>
<td style="text-align: center;">494</td>
<td style="text-align: center;">Stroke</td>
<td style="text-align: center;">0.0105867</td>
</tr>
<tr class="even">
<td style="text-align: left;">Glaucoma</td>
<td style="text-align: center;">0.0180</td>
<td style="text-align: center;">670</td>
<td style="text-align: center;">Glaucoma</td>
<td style="text-align: center;">0.0009629</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Ischemic heart disease</td>
<td style="text-align: center;">0.0500</td>
<td style="text-align: center;">493</td>
<td style="text-align: center;">Ischemic heart disease</td>
<td style="text-align: center;">0.0384710</td>
</tr>
<tr class="even">
<td style="text-align: left;">Non-alcoholic fatty liver disease</td>
<td style="text-align: center;">0.2050</td>
<td style="text-align: center;">1028</td>
<td style="text-align: center;">Total burden related to Non-alcoholic
fatty liver disease (NAFLD)</td>
<td style="text-align: center;">0.2262894</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Gastroesophageal reflux disease</td>
<td style="text-align: center;">0.1080</td>
<td style="text-align: center;">536</td>
<td style="text-align: center;">Gastroesophageal reflux disease</td>
<td style="text-align: center;">0.2107415</td>
</tr>
<tr class="even">
<td style="text-align: left;">Pre-eclampsia</td>
<td style="text-align: center;">0.0004</td>
<td style="text-align: center;">369</td>
<td style="text-align: center;">Maternal hypertensive disorders</td>
<td style="text-align: center;">0.0007386</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Preterm delivery</td>
<td style="text-align: center;">0.0010</td>
<td style="text-align: center;">381</td>
<td style="text-align: center;">Neonatal preterm birth</td>
<td style="text-align: center;">0.0072404</td>
</tr>
<tr class="even">
<td style="text-align: left;">Car accidents</td>
<td style="text-align: center;">0.0050</td>
<td style="text-align: center;">693</td>
<td style="text-align: center;">Motor vehicle road injuries</td>
<td style="text-align: center;">0.0107986</td>
</tr>
</tbody>
</table>

Following conditions are matched so that we calculated multiplier for
each condition to match original base values:

<table>
<colgroup>
<col style="width: 27%" />
<col style="width: 10%" />
<col style="width: 43%" />
<col style="width: 19%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">condition</th>
<th style="text-align: center;">prevalence</th>
<th style="text-align: center;">ihme_cause_name</th>
<th style="text-align: center;">multiplier-prevalence</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Diabetic retinopathy</td>
<td style="text-align: center;">0.026</td>
<td style="text-align: center;">Sense organ diseases</td>
<td style="text-align: center;">0.0259954</td>
</tr>
<tr class="even">
<td style="text-align: left;">Metabolic syndrome</td>
<td style="text-align: center;">0.330</td>
<td style="text-align: center;">Endocrine, metabolic, blood, and immune
disorders</td>
<td style="text-align: center;">0.3300877</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Erectile dysfunction</td>
<td style="text-align: center;">0.100</td>
<td style="text-align: center;">Urinary diseases and male
infertility</td>
<td style="text-align: center;">0.1000163</td>
</tr>
<tr class="even">
<td style="text-align: left;">Gastroesophageal reflux disease</td>
<td style="text-align: center;">0.108</td>
<td style="text-align: center;">Gastroesophageal reflux disease</td>
<td style="text-align: center;">0.1080213</td>
</tr>
</tbody>
</table>

### Obstructive Sleep Apnea

Obstructive Sleep Apnea (OSA) prevalence values are from Benjafield et
al. article. In the article Benjafield et al. estimated sleep apnea
prevalences for 30-69 years old population. We assume here that same
percentages apply for the 15-74 population.

In the calculator user can define obstructive sleep apnea prevalence
(Moderate-Severe AHI&gt;=15). This input value is used to estimate also
the mild sleep apnea prevalence of the population by adjusting the value
as it is presented in Armeni et al. article.

### Population Attributable Fraction

PAF can be calculated by knowing the risk ratio or odds ratio and
condition prevalence. Theory behind calculating PAF is presented in
[Cost-of-illness study of Obstructive Sleep Apnea Syndrome (OSAS) in
Italy](https://cergas.unibocconi.eu/sites/default/files/files/Cost-of-illness-study-of-Obstructive-Sleep-Apnea-Syndrome-%2528OSAS%2529-in-Italy_Report%25281%2529.pdf)
p. 15. More detailed how PAF can be calculated using Odd Ratio can be
found in [The Economic Cost Of Obstructive Sleep Apnea - A Systematic
Review](http://urn.fi/urn:nbn:fi:uef-20220894) p.19.

Example of calculating PAF for Type 2 diabetes with Risk Ratio:

    osa = 0.27 # moderate-severe osa rate
    RR = 1.63 
    (osa * (RR - 1) / (osa * (RR - 1) + 1)) = 0.1453722

Example of calculating PAF from Odd Ratio. Calculating PAF from OR is
more difficult so we generated a function with input parameters OR (odds
ratio), PD (disease prevalence), and PE (~sleep apnea prevalence).
Theory behind calculating PAF from Odds Ratio can be found in Armeni et
al. p.16.

    ## Give only decimals in parameters
    # OR = Odds Ratio
    # PD = having a disease, prevalence
    # PE = exposed, sleep apnea prevalence
    # (PE_ =  unexposed)
    paf_or <- function(OR, PD, PE){
      PD = PD * 100
      PE = PE * 100
      PE_ = 100 - PE
      VALUE1 = (PD * (1 - OR) + PE_ + OR * PE + sqrt( (PD * (1 - OR) + PE_ + OR * PE )^2 - 4 * PE_ * (1 - OR) *PD )) / (2 * PE_ * (1 - OR))
      VALUE2 = (PD * (1 - OR) + PE_ + OR * PE - sqrt( (PD * (1 - OR) + PE_ + OR * PE )^2 - 4 * PE_ * (1 - OR) *PD )) / (2 * PE_ * (1 - OR))
      VALUE <- ifelse(VALUE1 <= 100 & VALUE1 >= 0, VALUE1, VALUE2)
      PAF = 1 - ((100 * VALUE) / PD)
      return(PAF)
    }

## Cost calculation terms and formulas

*Sleep Apnea Prevalence (OSA)* = user input value or value from
Benjafield et al. article

*Condition Prevalence* = user input, value from IHME dataset or base
value from Armeni et al.

*PAF* = Population Attributable Fraction, calculated from Condition
prevalence and Risk Ratio/Odds Ratio

*Prevalent cases* = Population size x condition prevalence

*Prevalent cases influenced by osa* = Prevalent cases x Sleep Apnea
Prevalence (OSA)

*Annual costs (direct healthcare / direct non-healthcare / productivity
loss)* = user input or values from Armeni et al. article

## References

Alakörkkö I. (2022) The Economic Cost Of Obstructive Sleep Apnea - A
Systematic Review. Master Thesis in Medicine, University of Eastern
Finland. Available: <http://urn.fi/urn:nbn:fi:uef-20220894>

Armeni P, Borzoi L, Costa F, Donin G, and Gupta A. (2019)
*Cost-of-illness study of Obstructive Sleep Apnea Syndrome (OSAS) in
Italy*. Available:
<https://cergas.unibocconi.eu/sites/default/files/files/Cost-of-illness-study-of-Obstructive-Sleep-Apnea-Syndrome-%2528OSAS%2529-in-Italy_Report%25281%2529.pdf>

Benjafield AV, Ayas NT, Eastwood PR, Heinzer R, Ip MSM, Morrell MJ,
Nunez CM, Patel SR, Penzel T, Pépin JL, Peppard PE, Sinha S, Tufik S,
Valentine K, Malhotra A. (2019) *Estimation of the global prevalence and
burden of obstructive sleep apnoea: a literature-based analysis*. Lancet
Respir Med. 2019 Aug;7(8):687-698. doi: 10.1016/S2213-2600(19)30198-5.
Epub 2019 Jul 9

Borsoi L, Armeni P, Donin G, Costa F, and Ferini-Strambi L. (2021) “The
invisible costs of obstructive sleep apnea (OSA): a cost-of-illness
analysis,” PLoS ONE, pp. 1–23, 2021, doi: 10.1371/journal.pone.0268677.
