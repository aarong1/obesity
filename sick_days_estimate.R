headline_stats <- tibble::tribble(
~Key.Facts, ~`2019/2020`, ~`2020/2021`, ~`2021/2022`, ~`2022/2023`, ~`2023/2024`,
"Proportion of Staff with no recorded spells of Absence (%)",       50.743,       72.282,       61.964,       57.755,       56.945,
"Working Days Lost per Staff Year",        12.94,        9.785,       12.221,       12.343,       13.809,
"Percentage of Available Working Days Lost",        5.858,        4.381,        5.581,         5.68,        6.336,
"Total Number of Working Days Lost",   272796.762,   207159.541,   269839.468,   278104.891,    308601.83,
"Estimated Direct Salary Cost (£ million) [note 1]",        36.59,       28.386,       38.627,       39.049,       43.988,
"Average Number of Spells per Staff Year",        0.795,        0.389,         0.59,        0.664,        0.651,
"Proportion of Working Days Lost due to Certified absence (%)",       87.555,       90.946,       89.928,       88.869,       90.759,
"Proportion of Working Days Lost due to Self-certified absence (%)",       10.101,        4.675,        6.933,         8.85,        7.495,
"Proportion of Working Days Lost due to Uncertified/Missing absence (%)",        2.344,        4.378,        3.138,        2.282,        1.747,
"Proportion of Working Days Lost due to Long-term absence (%)",       77.031,       83.945,       80.467,       79.548,       82.592,
"Long-term Frequency Rate (%) [note 2]",       14.048,       11.369,       13.366,       13.088,       14.667,
"Long-term Average Duration (working days)",       62.784,       64.004,       64.336,       66.038,       68.868,
"Average Number of Short-term Spells per Staff Year",        0.637,         0.26,        0.438,        0.516,        0.485
  )

headline_stats

shortterm_absences <- tibble::tribble(
~`Reason.for.Short-Term.Absence`, ~`%.of.Spells`,
"Cold, Cough, Flu, Influenza",         28.761,
"Gastrointestinal Problems",         17.644,
"Chest and Respiratory Problems",         14.122,
"Anxiety/Stress/Depression/Other Psychiatric Illnesses",          8.803,
"Ear, Nose, Throat",          4.628,
"Injury, Fracture",          3.411,
"Back Problems",          3.392,
"Headache/Migraine",          3.171,
"Other Known Causes - Not Elsewhere Classified",          2.535,
"Genitourinary and Gynaecological Disorders",          2.111,
"Pregnancy Related Disorders",          1.927,
"Other Musculoskeletal Problems",           1.89,
"No Reason Specified",          1.558,
"Heart, Cardiac and Circulatory Problems",          1.291,
"Eye Problems",          1.226,
"Other",          3.531
  )


reason_absence <- tibble::tribble(
~Reason.for.Absence, ~`%.of.Working.Days.Lost`, ~`%.of.Spells`,
"Anxiety/Stress/Depression/Other Psychiatric Illnesses",                    43.044,         18.393,
"Injury, Fracture",                     6.954,          4.639,
"Gastrointestinal Problems",                     5.871,         14.496,
"Chest and Respiratory Problems",                     5.517,         11.664,
"Other Known Causes - Not Elsewhere Classified",                     4.807,          3.375,
"Heart, Cardiac and Circulatory Problems",                     4.387,          1.973,
"Cold, Cough, Flu, Influenza",                     4.368,         21.761,
"Other Musculoskeletal Problems",                     4.244,          2.509,
"Back Problems",                     3.953,           3.54,
"Benign and Malignant Tumours, Cancers",                     3.621,            1.1,
"Genitourinary and Gynaecological Disorders",                     2.458,          2.248,
"Pregnancy Related Disorders",                     1.894,          2.007,
"Ear, Nose, Throat",                     1.561,          3.691,
"Nervous System Disorders",                     1.316,          0.557,
"Eye Problems",                     1.101,          1.196,
"Other",                     3.347,           5.32,
"No Reason Specified",                     1.557,          1.533
  )

avg_duration_absence <- tibble::tribble(
~`Table.2:.Average.Duration.of.Absence.by.Reason`,
"Reason for Absence",
"Substance Abuse",
"Benign and Malignant Tumours, Cancers",
"Nervous System Disorders",
"Anxiety/Stress/Depression/Other Psychiatric Illnesses",
"Heart, Cardiac and Circulatory Problems",
"Other Musculoskeletal Problems",
"Blood Disorders",
"Injury, Fracture",
"Endocrine/Glandular Problems",
"Other Known Causes - Not Elsewhere Classified",
"Back Problems",
"Genitourinary and Gynaecological Disorders",
"Pregnancy Related Disorders",
"Eye Problems",
"Skin Disorders",
"Infectious Diseases",
"Asthma",
"Chest and Respiratory Problems",
"Burns, Poisoning, Frostbite, Hypothermia",
"Ear, Nose, Throat",
"Gastrointestinal Problems",
"Headache/Migraine",
"Dental and Oral Problems",
"Cold, Cough, Flu, Influenza",
"No Reason Specified"
                          )


long_term_absence <- tibble::tribble(
                                      ~Reason.for.Absence, ~`%.of.Long-term.Working.Days.Lost`, ~`%.of.Long-term.Spells`,
  "Anxiety/Stress/Depression/Other Psychiatric Illnesses",                              48.867,                   46.501,
                                       "Injury, Fracture",                               7.319,                    8.241,
          "Other Known Causes - Not Elsewhere Classified",                               4.993,                    5.836,
                "Heart, Cardiac and Circulatory Problems",                               4.913,                    3.972,
                              "Gastrointestinal Problems",                               4.678,                    5.269,
                         "Other Musculoskeletal Problems",                               4.574,                    4.323,
                  "Benign and Malignant Tumours, Cancers",                               4.263,                    3.269,
                                          "Back Problems",                               3.924,                    3.972,
                         "Chest and Respiratory Problems",                               3.562,                    4.458,
             "Genitourinary and Gynaecological Disorders",                               2.393,                    2.648,
                            "Pregnancy Related Disorders",                               1.779,                    2.243,
                               "Nervous System Disorders",                               1.503,                    1.216,
                                                  "Other",                               5.681,                    6.593,
                                    "No Reason Specified",                               1.552,                    1.459
  )


tibble::tribble(
~'Reason for Absence',    ~'No. of Days Lost per Staff Year2022/2023',    ~'No. of Days Lost per Staff Year2023/2024',    ~'Change',
  "Anxiety/Stress/Depression/Other Psychiatric Illnesses",   4.69,  5.944,  1.255,
                                                 "Asthma",  0.013,  0.023,   0.01,
                                          "Back Problems",  0.469,  0.546,  0.077,
                  "Benign and Malignant Tumours, Cancers",  0.546,    0.5, -0.046,
                                        "Blood Disorders",  0.072,  0.077,  0.005,
               "Burns, Poisoning, Frostbite, Hypothermia",  0.004,  0.004,      0,
                         "Chest and Respiratory Problems",  1.112,  0.762,  -0.35,
                            "Cold, Cough, Flu, Influenza",  0.581,  0.603,  0.022,
                               "Dental and Oral Problems",  0.022,  0.016, -0.006,
                                      "Ear, Nose, Throat",  0.195,  0.216,   0.02,
                           "Endocrine/Glandular Problems",  0.053,  0.073,   0.02,
                                           "Eye Problems",  0.116,  0.152,  0.036,
                              "Gastrointestinal Problems",  0.695,  0.811,  0.116,
             "Genitourinary and Gynaecological Disorders",  0.305,  0.339,  0.034,
                                      "Headache/Migraine",   0.12,  0.122,  0.002,
                "Heart, Cardiac and Circulatory Problems",  0.542,  0.606,  0.063,
                                    "Infectious Diseases",  0.039,  0.052,  0.014,
                                       "Injury, Fracture",  0.919,   0.96,  0.042,
                               "Nervous System Disorders",  0.142,  0.182,   0.04,
          "Other Known Causes - Not Elsewhere Classified",  0.693,  0.664, -0.029,
                         "Other Musculoskeletal Problems",  0.491,  0.586,  0.095,
                            "Pregnancy Related Disorders",  0.244,  0.262,  0.017,
                                         "Skin Disorders",  0.052,  0.055,  0.003,
                                        "Substance Abuse",  0.063,  0.041, -0.022,
                                    "No Reason Specified",  0.167,  0.215,  0.048,
                                           "NICS Overall", 12.343, 13.809,  1.466
  )


