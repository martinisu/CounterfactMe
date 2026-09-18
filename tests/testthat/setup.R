# Testene naar interne funksjoner direkte via ::: og gaar dermed utenom de
# eksporterte inngangspunktene, som alle kaller .load_data() foerst.
# Uten dette staar .cfm_env tom: .cfm_env$wealth_class_person er NULL,
# .wealth_class_from_nok() returnerer NA for alt, og wc$cut_nok[...] gir
# length 0 -- som expect_lt() avviser med "Result of comparison must be
# TRUE, FALSE, or NA".
CounterfactMe:::.load_data()
