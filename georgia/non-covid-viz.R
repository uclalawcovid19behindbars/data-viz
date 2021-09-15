library(tidyverse)
library(behindbarstools)


# staff vacancy rates -----------------------------------------------------
staff_vac_dat <- tibble(
    facility = c("Smith SP", "Telfair SP", "Georgia SP", "Lee Arrendale SP", "Ware SP",
                 "Calhoun SP", "Rogers SP", "Wilcox SP", "Autry SP", "Dodge SP"),
    vac_rate = c(.74, .727, .703, .685, .636, .706, .697, .685, .636, .625),
    security_lvl = c(rep("close", 5), rep("medium", 5))
)

# deaths by suicide -------------------------------------------------------
suicides <- tibble(
    facility = c("Lee Arrendale SP", "Augusta SMP", "Autry SP", "Baldwin SP",
                 "Central SP", "Georgia Diagnostic and Classification Prison",
                 "SMU", "Georgia SP", "Jefferson County CI", "Johnson SP",
                 "Lee SP", "Macon SP", "Phillips SP", "Pulaski SP", "Rogers SP",
                 "Rutledge SP", "Smith SP", "Smith Transitional Center", "Terrell County CI",
                 "Valdosta SP", "Wheeler SP", "Wilcox SP"),
    n_suicides = c(1, 1, 1, 1, 1, 3, 1, 10, 1, 2, 1, 1, 1, 1, 2, 1, 2, 1, 1, 3, 1, 1)
)

# deaths by homicide ------------------------------------------------------

homicides <- tibble(
    facility = c("Augusta SMP", "Baldwin SP",
                 "Central SP", "Coffee CF", "Georgia Diagnostic and Classification Prison",
                 "Hancock SP", "Hays SP", "Johnson SP",
                 "Macon SP", 
                 "Rutledge SP", "Smith SP", "Telfair SP", 
                 "Valdosta SP", "Wilcox SP"),
    n_homicides = c(3, 4, 1, 1, 4, 3, 2, 1, 9, 1, 7, 6, 3, 3)
)
