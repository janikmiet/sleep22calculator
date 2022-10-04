

## PAFOR yksikkotesti -----
## Essential hypertension, OSA: mild , 
# prevalence 0.303
# OR 1.18
# PAF 0.014306
#
## Armeni OSA: female 18 ja male 36

# TODO tarkista pafor laskukaava.
# TODO tarkista Armeni OSA arvo
paf_or(1.18, 0.303, 0.27)


##  Tee yksikkotesti RR erikseen /// TODO SELVITA onko kaava kopioitu oikein , desimaali vai prosentti
# PAFRR = ifelse(!is.na(RR), (osa_value() * (RR - 1) / (osa_value() * (RR - 1) + 1)), NA),

## Cancer , OVerall
osa = 0.27 # moderate-severe
RR = 1.4
(osa * (RR - 1) / (osa * (RR - 1) + 1))

## Type 2 diabetes / Moderate-Severe 14.5%
osa = 0.27 # moderate-severe
RR = 1.63 
(osa * (RR - 1) / (osa * (RR - 1) + 1))

##. Cardiovascular mortality / Severe 22.7 %
osa = 0.15 # severe
RR = 2.96
(osa * (RR - 1) / (osa * (RR - 1) + 1))

## Ischemic heart disease, severe 4.4% / moderate 8.6%
osa = 0.15 # severe
RR = 1.63
(osa * (RR - 1) / (osa * (RR - 1) + 1))
  
osa = 0.12 # moderate
RR = 1.38
(osa * (RR - 1) / (osa * (RR - 1) + 1))



## Stroke Severe / PAF 14.7 %
osa = 0.15 # severe
RR = 2.15
(osa * (RR - 1) / (osa * (RR - 1) + 1))

### Severe ei toimi, koska jotenkin populaatiota pitää osittaa. KAtsotaan suhteet
0.346/0.227
0.237/0.147
