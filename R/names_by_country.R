# ============================================================
# First names by country of origin (November 2026)
#
# names_by_region.csv pools 40 names per world region, so "ost_europa"
# mixes Polish, Russian, Lithuanian and Serbian names and
# "mena_sor_asia" mixes Arabic, Persian and Pakistani ones. The live site
# and the audits produced "Sergei fra Polen", "Tadas fra Serbia", "Milica
# fra Romania" and Hindus from India called Imran. This file gives the
# larger origin countries a pool in the right language.
#
# The lists are common given names in each language, compiled by hand;
# they are not frequency data and are not weighted. Countries not listed
# fall back to the region pool. Names are written without diacritics
# (R sources must be ASCII; the register writes them so too).
# ============================================================

.NAME_POOLS <- list(
  pl = list(M = c("Piotr", "Tomasz", "Andrzej", "Krzysztof", "Marek", "Pawel", "Grzegorz", "Jakub",
                  "Lukasz", "Mateusz", "Michal", "Kamil", "Adam", "Stanislaw"),
            F = c("Anna", "Katarzyna", "Magdalena", "Agnieszka", "Maria", "Joanna", "Beata", "Monika",
                  "Aleksandra", "Ewa", "Dorota", "Justyna")),
  lt = list(M = c("Andrius", "Tadas", "Mantas", "Darius", "Tomas", "Jonas", "Paulius", "Mindaugas"),
            F = c("Ieva", "Kristina", "Rasa", "Jurgita", "Egle", "Lina", "Agne", "Vilma")),
  lv = list(M = c("Janis", "Andris", "Martins", "Edgars", "Raimonds", "Sergei", "Aleksandr"),
            F = c("Inese", "Liga", "Ilze", "Kristine", "Anna", "Olga", "Elena")),
  ee = list(M = c("Andres", "Toomas", "Jaan", "Sergei", "Aleksandr"),
            F = c("Kristiina", "Liis", "Kadri", "Olga", "Natalia")),
  ru = list(M = c("Ivan", "Aleksandr", "Sergei", "Vladimir", "Mikhail", "Dmitri", "Andrei", "Yuri"),
            F = c("Olga", "Natalia", "Elena", "Tatiana", "Svetlana", "Ksenia", "Irina", "Anastasia")),
  ua = list(M = c("Oleksandr", "Volodymyr", "Andrii", "Serhii", "Dmytro", "Ivan", "Mykola", "Yurii"),
            F = c("Olena", "Oksana", "Iryna", "Natalia", "Tetiana", "Yulia", "Anastasiia", "Kateryna")),
  bks = list(M = c("Marko", "Nikola", "Stefan", "Milan", "Dragan", "Ivan", "Amir", "Emir", "Adnan", "Haris"),
             F = c("Vesna", "Milica", "Jovana", "Ana", "Ivana", "Jelena", "Amra", "Lejla", "Maja", "Sanja")),
  al = list(M = c("Arben", "Besnik", "Driton", "Valon", "Ilir", "Arton", "Fatmir", "Blerim"),
            F = c("Arta", "Besa", "Drita", "Valbona", "Teuta", "Mimoza", "Elira", "Blerta")),
  ro = list(M = c("Andrei", "Alexandru", "Mihai", "Ionut", "Marius", "Florin", "Gabriel", "Cristian"),
            F = c("Elena", "Maria", "Ioana", "Andreea", "Alina", "Mihaela", "Cristina", "Gabriela")),
  bg = list(M = c("Georgi", "Dimitar", "Ivan", "Nikolay", "Petar", "Stoyan"),
            F = c("Maria", "Ivanka", "Elena", "Desislava", "Petya", "Yordanka")),
  gr = list(M = c("Georgios", "Dimitrios", "Konstantinos", "Nikolaos", "Ioannis", "Panagiotis"),
            F = c("Maria", "Eleni", "Katerina", "Sofia", "Georgia", "Dimitra")),
  hu = list(M = c("Laszlo", "Zoltan", "Gabor", "Peter", "Istvan", "Attila"),
            F = c("Katalin", "Erzsebet", "Zsuzsanna", "Anna", "Eszter", "Ildiko")),
  cs = list(M = c("Jan", "Petr", "Tomas", "Martin", "Pavel", "Lukas", "Marek", "Peter"),
            F = c("Jana", "Petra", "Lucie", "Eva", "Zuzana", "Martina", "Katarina")),
  ar = list(M = c("Mohammed", "Ahmed", "Ali", "Omar", "Hassan", "Ibrahim", "Yousef", "Mahmoud",
                  "Khalid", "Mustafa", "Karim", "Tariq"),
            F = c("Aisha", "Fatima", "Khadija", "Layla", "Maryam", "Zainab", "Noor", "Hala", "Rania",
                  "Samira", "Yasmin", "Amira", "Hiba", "Salma")),
  fa = list(M = c("Reza", "Hossein", "Ali", "Mehdi", "Amir", "Farid", "Saeed", "Arash", "Babak"),
            F = c("Maryam", "Zahra", "Leila", "Sara", "Fatemeh", "Shirin", "Nasrin", "Parisa", "Mina", "Dunya")),
  af = list(M = c("Mohammad", "Ahmad", "Ali", "Hamid", "Jawad", "Mustafa", "Farid", "Rahim"),
            F = c("Fatima", "Zahra", "Maryam", "Shabnam", "Nadia", "Mariam", "Parwana", "Hasina")),
  pk = list(M = c("Muhammad", "Imran", "Faisal", "Bilal", "Adnan", "Tariq", "Ali", "Usman", "Kamran", "Rizwan"),
            F = c("Aisha", "Sana", "Zainab", "Noor", "Fatima", "Ayesha", "Saima", "Nadia", "Hina", "Rabia")),
  hi = list(M = c("Raj", "Rahul", "Amit", "Vikram", "Arjun", "Sanjay", "Ravi", "Anil", "Suresh", "Gurpreet"),
            F = c("Priya", "Anjali", "Pooja", "Neha", "Sunita", "Kavita", "Deepa", "Harpreet", "Asha", "Lakshmi")),
  ta = list(M = c("Kumar", "Suresh", "Rajan", "Ganesh", "Sivakumar", "Tharshan", "Niroshan", "Prakash"),
            F = c("Lakshmi", "Kavitha", "Shanthi", "Thanuja", "Nisha", "Priyanka", "Sharmila", "Vasuki")),
  ne = list(M = c("Ram", "Bikash", "Suman", "Prakash", "Ramesh", "Sanjay", "Dipak", "Nabin"),
            F = c("Sita", "Sunita", "Anita", "Gita", "Sarita", "Laxmi", "Puja", "Rita")),
  tr = list(M = c("Mehmet", "Mustafa", "Ahmet", "Ali", "Huseyin", "Hasan", "Murat", "Emre", "Yusuf", "Ibrahim"),
            F = c("Ayse", "Fatma", "Emine", "Hatice", "Zeynep", "Elif", "Meryem", "Esra", "Selin", "Hulya")),
  vi = list(M = c("Anh", "Bao", "Duc", "Hoang", "Khanh", "Long", "Minh", "Nam", "Phong", "Quan", "Tuan", "Vinh"),
            F = c("Linh", "Mai", "Huong", "Hoa", "Lan", "Phuong", "Thao", "Trang", "Thuy", "Yen")),
  zh = list(M = c("Wei", "Jun", "Hao", "Liang", "Ming", "Jian", "Lei", "Tao"),
            F = c("Mei", "Lin", "Xiu", "Hui", "Ying", "Jing", "Fang", "Li")),
  th = list(M = c("Somchai", "Anan", "Niran", "Saiyud", "Somsak", "Prasert", "Chai", "Wichai"),
            F = c("Suda", "Apinya", "Pim", "Ploy", "Siriporn", "Nong", "Malee", "Ratana")),
  ja = list(M = c("Hiroshi", "Takeshi", "Kenji", "Yuki", "Daisuke", "Kenta"),
            F = c("Yuko", "Keiko", "Akiko", "Yumi", "Naoko", "Haruka")),
  ko = list(M = c("Min-jun", "Ji-hoon", "Seung-ho", "Hyun-woo", "Dong-hyun"),
            F = c("Ji-yeon", "Soo-jin", "Min-ji", "Hye-jin", "Eun-ji")),
  my = list(M = c("Saw", "Htoo", "Aung", "Kyaw", "Zaw", "Thein"),
            F = c("Naw", "Mu", "Hla", "Thida", "Khin", "Aye")),
  id = list(M = c("Budi", "Agus", "Dedi", "Rizky", "Andi"),
            F = c("Siti", "Dewi", "Sri", "Putri", "Ayu")),
  mn = list(M = c("Bat", "Ganbold", "Erdene", "Bold"),
            F = c("Oyun", "Saran", "Altantsetseg", "Bolormaa")),
  es = list(M = c("Carlos", "Juan", "Jose", "Luis", "Miguel", "Antonio", "Diego", "Pedro", "Manuel",
                  "Francisco", "Pablo", "Alejandro", "Andres", "Hector"),
            F = c("Maria", "Sofia", "Camila", "Lucia", "Valentina", "Isabel", "Andrea", "Gabriela",
                  "Daniela", "Ana", "Carolina", "Paula", "Adriana", "Ximena")),
  pt = list(M = c("Joao", "Pedro", "Lucas", "Gabriel", "Rafael", "Thiago", "Bruno", "Felipe"),
            F = c("Ana", "Maria", "Juliana", "Fernanda", "Camila", "Beatriz", "Larissa", "Patricia")),
  ph = list(M = c("Jose", "Mark", "John", "Michael", "Ramon", "Rodel", "Jerome", "Arnel"),
            F = c("Maria", "Marites", "Jocelyn", "Rosalie", "Analyn", "Cristina", "Rowena", "Lovely")),
  so = list(M = c("Mohamed", "Abdi", "Abdullahi", "Ahmed", "Hassan", "Ismail", "Yusuf", "Ali", "Abdirahman", "Omar"),
            F = c("Hawa", "Amina", "Khadra", "Fadumo", "Asha", "Halima", "Saido", "Ayan", "Hodan", "Sahra")),
  ha = list(M = c("Yohannes", "Tesfaye", "Robel", "Samuel", "Bereket", "Daniel", "Dawit", "Haile", "Mulugeta", "Filmon"),
            F = c("Senait", "Hiwet", "Salem", "Tigist", "Almaz", "Helen", "Selam", "Meron", "Saba", "Hanna")),
  wa = list(M = c("Kwame", "Kofi", "Adebayo", "Chinedu", "Emmanuel", "Samuel", "Joseph", "Ibrahim"),
            F = c("Akosua", "Abena", "Adaeze", "Nneka", "Grace", "Fatmata", "Esther", "Blessing")),
  ea = list(M = c("Tendai", "Sipho", "Joseph", "John", "Peter", "Emmanuel", "Jean", "Eric", "David", "Moses"),
            F = c("Tariro", "Nomvula", "Grace", "Mary", "Esther", "Aline", "Ruth", "Faith", "Joyce", "Agnes"))
)

# Country label (as in immigrant_country_dist.csv) -> pool.
.COUNTRY_NAME_POOL <- c(
  "Polen" = "pl", "Litauen" = "lt", "Latvia" = "lv", "Estland" = "ee",
  "Russland" = "ru", "Belarus" = "ru", "Ukraina" = "ua", "Moldova" = "ro",
  "Bosnia-Hercegovina" = "bks", "Kroatia" = "bks", "Serbia" = "bks", "Montenegro" = "bks",
  "Slovenia" = "bks", "Nord-Makedonia" = "bks", "Albania" = "al", "Kosovo" = "al",
  "Romania" = "ro", "Bulgaria" = "bg", "Hellas" = "gr", "Ungarn" = "hu",
  "Tsjekkia" = "cs", "Slovakia" = "cs",
  "Egypt" = "ar", "Irak" = "ar", "Jordan" = "ar", "Libanon" = "ar", "Marokko" = "ar",
  "Saudi-Arabia" = "ar", "Syria" = "ar", "Tunisia" = "ar", "Sudan" = "ar", "Jemen" = "ar",
  "Algerie" = "ar", "Brunei" = "ar",
  "Iran" = "fa", "Afghanistan" = "af", "Pakistan" = "pk", "Bangladesh" = "pk",
  "India" = "hi", "Sri Lanka" = "ta", "Nepal" = "ne", "Tyrkia" = "tr",
  "Vietnam" = "vi", "Kina" = "zh", "Hongkong" = "zh", "Taiwan" = "zh",
  "Thailand" = "th", "Laos" = "th", "Japan" = "ja", "S\u00f8r-Korea" = "ko",
  "Myanmar" = "my", "Indonesia" = "id", "Mongolia" = "mn",
  "Argentina" = "es", "Bolivia" = "es", "Chile" = "es", "Colombia" = "es", "Cuba" = "es",
  "Ecuador" = "es", "Mexico" = "es", "Paraguay" = "es", "Peru" = "es", "Uruguay" = "es",
  "Venezuela" = "es", "Brasil" = "pt", "Filippinene" = "ph",
  "Somalia" = "so", "Eritrea" = "ha", "Etiopia" = "ha",
  "Ghana" = "wa", "Nigeria" = "wa", "Liberia" = "wa", "Sierra Leone" = "wa",
  "Kenya" = "ea", "Tanzania" = "ea", "Uganda" = "ea", "Rwanda" = "ea", "Burundi" = "ea",
  "S\u00f8r-Afrika" = "ea", "Mosambik" = "ea", "Angola" = "ea", "S\u00f8r-Sudan" = "ea"
)

# A first name from the region pool, or from the origin country's pool
# when the country of the life being drawn is known and belongs to that
# region. counterfact_me() sets .cfm_env$name_country for the duration of
# one draw (see .counterfact_conditional).
.region_name <- function(region, gender) {
  g <- if (identical(toupper(gender %||% ""), "F")) "F" else "M"
  ctry <- .cfm_env$name_country
  key <- if (!is.null(ctry) && !is.na(ctry)) unname(.COUNTRY_NAME_POOL[ctry]) else NA_character_
  if (!is.na(key) && identical(.cfm_env$name_country_region, region)) {
    pool <- .NAME_POOLS[[key]][[g]]
    if (length(pool)) return(sample(pool, 1))
  }
  nbr <- .cfm_env$names_by_region
  if (is.null(nbr)) return(NULL)
  pool <- nbr$name[nbr$region == region & nbr$gender == g]
  if (!length(pool)) return(NULL)
  pool[sample.int(length(pool), 1)]
}
