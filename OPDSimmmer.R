library(simmer)
library(simmer.plot)
library(parallel)
library(tidyverse)


env <- simmer("OPD")

new_doc_time <- 10
new_cons_time <- 3
return_doc_time <- 5
return_cons_time <- 3
dressing_time <- 20


Dressing_Patient <- trajectory("dressing patients' path") %>%
  ## add an dressing activity 
  seize("nurse", 1) %>%
  timeout(function() rnorm(1, dressing_time, 5)) %>%
  release("nurse", 1) %>%
  ## add a planning activity
  seize("administration", 1) %>%
  timeout(function() rnorm(1, 1)) %>%
  release("administration", 1)

New_Patient <- trajectory("new patients' path") %>%
  ## add a doctor activity
  seize("doctor", 1) %>%
  timeout(function() rnorm(1, new_doc_time, 2)) %>%
  ## add a consultation activity
  seize("consultant", 1) %>%
  timeout(function() rnorm(1, new_cons_time)) %>%
  release("doctor", 1) %>%
  release("consultant", 1) %>%
  ## add a planning activity
  seize("administration", 1) %>%
  timeout(function() rnorm(1, 1)) %>%
  release("administration", 1)


Return_Patient <- trajectory("return patients' path") %>%
  ## add a consultation activity
  seize("doctor", 1) %>%
  timeout(function() rnorm(1, return_doc_time)) %>%
  ## add a consultation activity
  seize("consultant", 1) %>%
  timeout(function() rnorm(1, 3)) %>%
  release("consultant", 1) %>%
  release("doctor", 1) %>%
  ## add a planning activity
  seize("administration", 1) %>%
  timeout(function() rnorm(1, 1)) %>%
  release("administration", 1)

# env %>%
#   add_resource("nurse", 3) %>%
#   add_resource("doctor", 1) %>%
#   add_resource("consultant", 1) %>%
#   add_resource("administration", 1) %>%
#   add_generator("dressing patient", Dressing_Patient, function() rnorm(1, 5, 0)) %>%
#   add_generator("return patient", Return_Patient, function() rnorm(1,1,0)) %>%
#   add_generator("new patient", New_Patient, function() rnorm(1,2,0))
# 
# env %>%
#   run(180) %>%
#   now()
# 
# 
# env %>% peek(Inf, verbose=TRUE)


runlength <- 160

envs <- mclapply(1:1000, function(i) {
  simmer("OPD") %>%
    add_resource("nurse", 2) %>%
    add_resource("doctor", 4, preemptive = TRUE) %>%
    add_resource("consultant", 1) %>%
    add_resource("administration", 1) %>%
    add_generator("return", Return_Patient, from_to(c(0,0,0),runlength,
                                              dist = function () {return_doc_time}, arrive = TRUE, every = runlength), priority = 0) %>%
    add_generator("new", New_Patient, from_to(c(0,0,0),runlength,
                                              dist = function () {new_doc_time}, arrive = TRUE, every = runlength), priority = 0) %>%
    run(runlength) %>%
    wrap()
})

envs %>%
  get_mon_arrivals()

result <- envs %>% 
  get_mon_arrivals() %>%
  transform(waiting_time = end_time - start_time - activity_time) %>%
  group_by(replication, New = str_detect(name, "new")) %>%
  tally() %>%
  pivot_wider(id_cols = replication, names_from = New , values_from = n) %>%
  select(Trial = replication, ReturnPatients = 'FALSE', NewPatients = 'TRUE')

result %>%
  summary()


plot(get_mon_resources(envs), metric = "utilization")