
system("docker build  --no-cache -t sleep22calculator . ")
system("docker run --name shiny_sleep22calculator --rm -d -p 3838:3838 sleep22calculator") # run container
# system("docker stop shiny_container")
