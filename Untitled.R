set.seed(123)

n <- 300

patient_id <- 1:n
age <- round(rnorm(n, mean = 50, sd = 15))
age[age < 18] <- 18

sex <- sample(c("Male", "Female"), n, replace = TRUE)
clinic <- sample(c("Clinic A", "Clinic B", "Clinic C"), n, replace = TRUE)
bmi <- round(rnorm(n, mean = 27, sd = 4), 1)

sbp <- round(rnorm(n, mean = 135, sd = 15))
dbp <- round(rnorm(n, mean = 85, sd = 10))

smoker <- sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.3, 0.7))

start_date <- as.Date("2023-01-01")
end_date <- as.Date("2024-12-31")
visit_date <- as.Date(runif(n, min = as.numeric(start_date),
                            max = as.numeric(end_date)),
                      origin = "1970-01-01")

clinic_data <- data.frame(
  patient_id, age, sex, clinic, bmi, sbp, dbp, smoker, visit_date
)

if (!dir.exists("data")) dir.create("data")

write.csv(clinic_data, "data/clinic_data.csv", row.names = FALSE)

head(clinic_data)

