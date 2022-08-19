var_order <- c("data", "age_range", "Age", "AgeInMonths", "Gender", "ParentHighestEducation", "HighestEducation", 
               "Ethnicity", "Region")
cat_order <- c(
  # data
  NA, "SM", "Qual", "Sp", "Daycare", "In-house-Eng", "In-house-Sp", "In-house-Alt",
  # AgeInMonths
  NA, '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', 
  '23', '24', '25', '26', '27', '28', '29', '30', '31', '32', '33', '34', '35', '36',
  # age_range
  NA, "03.5 to 6 mo", "03.5 to 10 mo", "4 to 10 mo", "07 to 10.5 mo", "09.5 to 20 mo",  
  "11 to 20 mo", "11 to 31.5 mo", "21 to 30 mo",
  "21 to 31.5 mo", "2 to 4 years", "5 years", "5 to 8 years", "9 to 12 years", "12 to 13 years", 
  "14 to 15 years", "16 to 17 years", "18 to 21 years", "21.00 to 30.99 years", "31.00 to 40.99 years", 
  "41.00 to 50.99 years", "51.00 to 64.99 years", "65.00 to 99.99 years",
  # Age
  "2", "3", "4", "5",
  # Gender
  NA, "Male", "Female",
  # ParentHighestEducation & HighestEducation
  NA, "Did not complete high school (no diploma)", "High school graduate (including GED)", 
  "Some college or associate degree", "Bachelor's degree or higher",
  # Ethnicity
  NA, "Hispanic", "Asian", "Black", "White", "AmericanIndAlaskanNat", 
  "NativeHawPacIsl", "MultiRacial", "Other",
  # Region
  NA, "northeast", "midwest", "south", "west")

