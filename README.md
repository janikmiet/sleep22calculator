# Sleep Apnea Cost Top-Down Calculator

*Sleep Apnea Cost calculator is an application which estimates cost of
sleep apnea by the prevalences of medical conditions connected to sleep
apnea. This project has received funding from the European Union’s
Horizon 2020 research and innovation programme under grant agreement no.
965417. This document describes data and methods used in sleep apnea top
to down calculation. More detailed playbook of the project can be found
at <https://research.janimiettinen.fi/material/sleep22/>.*

- Check project method website: <https://research.janimiettinen.fi/material/sleep22/>

- Working app available: <https://janimiettinen.shinyapps.io/sleepapneacalculator/>

- Dockerhub image: <https://hub.docker.com/repository/docker/janikmiet/sleeprevolution_calculator/general>


## Installation / App Launch

### Terminal App Run

```
R -e "shiny::runApp('app/')"
```


### Docker Image

Use docker to launch app

```
docker build  --no-cache -t sleep22calculator . 
docker run --name shiny_sleep22calculator --rm -d -p 3838:3838 sleep22calculator
```

### ShinyServer / rsconnect

Modify and use script `deploy.R`



# Acknowledgement

![](app/www/img/alllogos.png) 
<br>
<img src="app/www/img/uef.png" style="width:4.0%" />