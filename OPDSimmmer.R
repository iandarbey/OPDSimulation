library(simmer)
library(simmer.plot)
library(parallel)
library(tidyverse)

##OPD Parameters

#Patient Review Time
new_doc_time <- 10
new_cons_time <- 3
return_doc_time <- 5
return_cons_time <- 3
dressing_time <- 20
#Patient Priority
new_priority <- 10
return_priority <- 10
dressing_prirority <- 10
#Clinic Session Details
OPD_Clinic_Time <- 160
new_patient_first_slot <- 3
return_patient_first_slot <- 3
number_of_trials <- 100
#Resources
Doctors <- 4
Nurses <- 2
Consultants <- 1
Administrators <- 1



#Setup Environment for Simulation
env <- simmer("OPD")


#Make a Dressing Patient Pathway
Dressing_Patient <- trajectory("dressing patients' path") %>%
  ## add an dressing activity 
  seize("nurse", 1) %>%
  timeout(function() rnorm(1, dressing_time, 5)) %>%
  release("nurse", 1) %>%
  ## add a planning activity
  seize("administration", 1) %>%
  timeout(function() rnorm(1, 1)) %>%
  release("administration", 1)


#Make a New Patient Pathway
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


#Make a Return Patient Pathway
Return_Patient <- trajectory("return patients' path") %>%
  ## add a consultation activity
  seize("doctor", 1) %>%
  timeout(function() rnorm(1, return_doc_time)) %>%
  ## add a consultation activity
  seize("consultant", 1) %>%
  timeout(function() rnorm(1, 3)) %>%
  release("doctor", 1) %>%
  release("consultant", 1) %>%
  ## add a planning activity
  seize("administration", 1) %>%
  timeout(function() rnorm(1, 1)) %>%
  release("administration", 1)



#Simulate The OPD Session
envs <- mclapply(1:number_of_trials, function(i) {
  simmer("OPD") %>%
    add_resource("nurse", Nurses) %>%
    add_resource("doctor", Doctors, preemptive = TRUE) %>%
    add_resource("consultant", Consultants) %>%
    add_resource("administration", Administrators) %>%
    add_generator("return", Return_Patient, from_to(rep(0,return_patient_first_slot),OPD_Clinic_Time,
                                              dist = function () {return_doc_time}, arrive = TRUE,
                                              every = OPD_Clinic_Time), priority = return_priority) %>%
    add_generator("new", New_Patient, from_to(rep(0,new_patient_first_slot),OPD_Clinic_Time,
                                              dist = function () {new_doc_time}, arrive = TRUE,
                                              every = OPD_Clinic_Time), priority = new_priority) %>%
    run(OPD_Clinic_Time) %>%
    wrap()
})


#View Patient Pathway
envs %>%
  get_mon_arrivals(per_resource = TRUE) #%>%  view()


#Save the Results of How Many Patients were seen
result <- envs %>% 
  get_mon_arrivals() %>%
  group_by(replication, New = str_detect(name, "new")) %>%
  tally() %>%
  pivot_wider(id_cols = replication, names_from = New , values_from = n) %>%
  select(Trial = replication, ReturnPatients = 'FALSE', NewPatients = 'TRUE')

result %>%
  summary()


plot(get_mon_resources(envs), metric = "utilization")
