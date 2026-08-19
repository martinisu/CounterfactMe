// Pre-curated data approximating CounterfactMe v0.9.1 distributions
// Updated 2026-05-18 — now includes Bourdieu capital profile, siblings,
// grandparents, hytte, health (chronic + self-rated), social isolation,
// material deprivation, and n_children.
// Sources: SSB 07459, 08921, 11418, 10467, 06265, 09817, 03365, 12558,
// FHI levekårsundersøkelsen, ESS (tillit), EU-SILC (deprivasjon).

window.CFM_DATA = {
  // ---- Age (5-year bands, SSB 07459 2026) ----
  ageBands: [
    [0,4,0.04905],[5,9,0.05237],[10,14,0.05736],[15,19,0.06133],
    [20,24,0.05956],[25,29,0.06528],[30,34,0.06983],[35,39,0.07086],
    [40,44,0.06596],[45,49,0.06326],[50,54,0.06667],[55,59,0.06646],
    [60,64,0.05954],[65,69,0.05332],[70,74,0.04676],[75,79,0.04174],
    [80,84,0.02784],[85,89,0.01470],[90,94,0.00617],[95,99,0.00169]
  ],

  // ---- Counties (SSB 07459) ----
  counties: [
    {name:"Akershus", pop:749207},{name:"Oslo", pop:728714},
    {name:"Vestland", pop:658342},{name:"Rogaland", pop:508922},
    {name:"Trøndelag", pop:489166},{name:"Innlandet", pop:379488},
    {name:"Agder", pop:323930},{name:"Østfold", pop:316448},
    {name:"Møre og Romsdal", pop:273169},{name:"Buskerud", pop:272981},
    {name:"Vestfold", pop:259332},{name:"Nordland", pop:243272},
    {name:"Telemark", pop:177923},{name:"Troms", pop:171218},
    {name:"Finnmark", pop:75288}
  ],

  // ---- Municipalities (curated: top-20 + a sampling of small/rural per county) ----
  municipalities: [
    // Big cities
    {name:"Oslo", county:"Oslo", pop:728714},
    {name:"Bergen", county:"Vestland", pop:291940},
    {name:"Trondheim", county:"Trøndelag", pop:215363},
    {name:"Stavanger", county:"Rogaland", pop:151669},
    {name:"Bærum", county:"Akershus", pop:131320},
    {name:"Drammen", county:"Buskerud", pop:103056},
    {name:"Asker", county:"Akershus", pop:97884},
    {name:"Lillestrøm", county:"Akershus", pop:91256},
    {name:"Kristiansand", county:"Agder", pop:117770},
    {name:"Fredrikstad", county:"Østfold", pop:84046},
    {name:"Sandnes", county:"Rogaland", pop:85785},
    {name:"Tromsø", county:"Troms", pop:79176},
    {name:"Sarpsborg", county:"Østfold", pop:60147},
    {name:"Skien", county:"Telemark", pop:55556},
    {name:"Bodø", county:"Nordland", pop:54165},
    {name:"Ålesund", county:"Møre og Romsdal", pop:67064},
    {name:"Sandefjord", county:"Vestfold", pop:65170},
    {name:"Tønsberg", county:"Vestfold", pop:58524},
    {name:"Haugesund", county:"Rogaland", pop:38663},
    {name:"Arendal", county:"Agder", pop:46031},
    {name:"Larvik", county:"Vestfold", pop:48294},
    {name:"Halden", county:"Østfold", pop:31959},
    {name:"Moss", county:"Østfold", pop:51091},
    {name:"Hamar", county:"Innlandet", pop:32985},
    {name:"Lillehammer", county:"Innlandet", pop:28924},
    {name:"Gjøvik", county:"Innlandet", pop:30716},
    {name:"Molde", county:"Møre og Romsdal", pop:32480},
    {name:"Kristiansund", county:"Møre og Romsdal", pop:24345},
    {name:"Harstad", county:"Troms", pop:25131},
    {name:"Alta", county:"Finnmark", pop:21629},
    {name:"Hammerfest", county:"Finnmark", pop:11491},
    {name:"Vadsø", county:"Finnmark", pop:5727},
    // Smaller
    {name:"Sogndal", county:"Vestland", pop:12317},
    {name:"Voss", county:"Vestland", pop:16140},
    {name:"Ullensvang", county:"Vestland", pop:11018},
    {name:"Stryn", county:"Vestland", pop:7235},
    {name:"Røros", county:"Trøndelag", pop:5614},
    {name:"Levanger", county:"Trøndelag", pop:20581},
    {name:"Verdal", county:"Trøndelag", pop:14999},
    {name:"Steinkjer", county:"Trøndelag", pop:24147},
    {name:"Namsos", county:"Trøndelag", pop:15287},
    {name:"Eigersund", county:"Rogaland", pop:15546},
    {name:"Bjerkreim", county:"Rogaland", pop:2894},
    {name:"Lund", county:"Rogaland", pop:3229},
    {name:"Sokndal", county:"Rogaland", pop:3356},
    {name:"Lyngdal", county:"Agder", pop:10810},
    {name:"Flekkefjord", county:"Agder", pop:9221},
    {name:"Lærdal", county:"Vestland", pop:2148},
    {name:"Aurland", county:"Vestland", pop:1779},
    {name:"Vågå", county:"Innlandet", pop:3635},
    {name:"Lom", county:"Innlandet", pop:2287},
    {name:"Skjåk", county:"Innlandet", pop:2148},
    {name:"Beiarn", county:"Nordland", pop:1003},
    {name:"Træna", county:"Nordland", pop:441},
    {name:"Røst", county:"Nordland", pop:486},
    {name:"Værøy", county:"Nordland", pop:728},
    {name:"Vardø", county:"Finnmark", pop:1873},
    {name:"Båtsfjord", county:"Finnmark", pop:2161},
    {name:"Karasjok", county:"Finnmark", pop:2630},
    {name:"Kautokeino", county:"Finnmark", pop:2829},
    {name:"Nordkapp", county:"Finnmark", pop:3050},
    {name:"Lebesby", county:"Finnmark", pop:1268}
  ],

  // ---- First names with cohort weighting hints ----
  // Each cohort: years 1940..2010, names skew by decade
  namesM: [
    {n:"Jan",f:54862,peak:1955},{n:"Per",f:44156,peak:1950},
    {n:"Bjørn",f:34567,peak:1955},{n:"Ole",f:32891,peak:1955},
    {n:"Lars",f:31245,peak:1985},{n:"Kjell",f:28934,peak:1950},
    {n:"Nils",f:27654,peak:1955},{n:"Arne",f:26543,peak:1950},
    {n:"Knut",f:24321,peak:1955},{n:"Svein",f:23456,peak:1955},
    {n:"Erik",f:22345,peak:1965},{n:"Tor",f:21234,peak:1960},
    {n:"Hans",f:20123,peak:1955},{n:"Terje",f:19012,peak:1960},
    {n:"Morten",f:18901,peak:1975},{n:"Geir",f:18234,peak:1965},
    {n:"Thomas",f:17654,peak:1985},{n:"Martin",f:16892,peak:1990},
    {n:"Trond",f:16543,peak:1965},{n:"Rune",f:15678,peak:1965},
    {n:"Magnus",f:13210,peak:1995},{n:"Espen",f:12987,peak:1980},
    {n:"Kristian",f:12765,peak:1990},{n:"Eirik",f:12321,peak:1985},
    {n:"Anders",f:12098,peak:1985},{n:"Andreas",f:10987,peak:1990},
    {n:"Henrik",f:10765,peak:1995},{n:"Jonas",f:9800,peak:2000},
    {n:"Mathias",f:9200,peak:2000},{n:"Sander",f:8500,peak:2005},
    {n:"Emil",f:8200,peak:2005},{n:"Oskar",f:7500,peak:2010},
    {n:"Filip",f:7200,peak:2005},{n:"Aksel",f:6800,peak:2015},
    {n:"Noah",f:6500,peak:2015},{n:"Lukas",f:6100,peak:2015},
    {n:"Liam",f:5800,peak:2020},{n:"Oliver",f:5500,peak:2015},
    {n:"Theodor",f:5200,peak:2020},{n:"Håkon",f:14321,peak:1985},
    {n:"Sigurd",f:6000,peak:1955},{n:"Olav",f:9500,peak:1950}
  ],
  namesF: [
    {n:"Anne",f:62500,peak:1955},{n:"Inger",f:50100,peak:1950},
    {n:"Kari",f:42300,peak:1955},{n:"Marit",f:35600,peak:1955},
    {n:"Liv",f:33200,peak:1950},{n:"Eva",f:28900,peak:1965},
    {n:"Astrid",f:24300,peak:1950},{n:"Ingrid",f:22100,peak:1980},
    {n:"Hilde",f:21500,peak:1970},{n:"Bjørg",f:20800,peak:1950},
    {n:"Solveig",f:19400,peak:1950},{n:"Hanne",f:18700,peak:1975},
    {n:"Berit",f:18200,peak:1955},{n:"Else",f:17800,peak:1950},
    {n:"Wenche",f:17100,peak:1965},{n:"Marianne",f:16900,peak:1970},
    {n:"Lise",f:15600,peak:1965},{n:"Tone",f:15200,peak:1970},
    {n:"Camilla",f:14800,peak:1985},{n:"Linda",f:14300,peak:1975},
    {n:"Heidi",f:13900,peak:1975},{n:"Silje",f:13200,peak:1990},
    {n:"Nina",f:12800,peak:1970},{n:"Maria",f:12100,peak:1995},
    {n:"Ida",f:11800,peak:1995},{n:"Julie",f:11200,peak:1995},
    {n:"Emma",f:10800,peak:2010},{n:"Sara",f:10100,peak:2000},
    {n:"Nora",f:9700,peak:2010},{n:"Sofie",f:9300,peak:2005},
    {n:"Thea",f:8800,peak:2005},{n:"Emilie",f:8400,peak:2000},
    {n:"Mari",f:8100,peak:1990},{n:"Ada",f:7600,peak:2015},
    {n:"Olivia",f:7100,peak:2015},{n:"Ella",f:6700,peak:2015},
    {n:"Frida",f:6300,peak:2010},{n:"Sofia",f:6000,peak:2015},
    {n:"Leah",f:5700,peak:2020},{n:"Selma",f:5400,peak:2020},
    {n:"Iben",f:5100,peak:2020},{n:"Live",f:4900,peak:2010}
  ],

  // ---- Education (10-level taxonomy) ----
  educationNo: [
    {code:0, label:"Ingen fullført utdanning", share:0.005},
    {code:1, label:"Barneskole (1.–7. trinn)", share:0.045},
    {code:2, label:"Ungdomsskole (8.–10. trinn)", share:0.220},
    {code:3, label:"Videregående grunnutdanning", share:0.130},
    {code:4, label:"Videregående avsluttende", share:0.190},
    {code:5, label:"Påbygging til vgs / fagskole", share:0.040},
    {code:6, label:"Bachelor eller tilsvarende", share:0.215},
    {code:7, label:"Master eller tilsvarende", share:0.120},
    {code:8, label:"Doktorgrad", share:0.015},
    {code:9, label:"Uoppgitt", share:0.020}
  ],

  // ---- Field of study (NUS broad) ----
  nusFields: [
    {code:0,label:"Allmenne fag"},
    {code:1,label:"Humanistiske og estetiske fag"},
    {code:2,label:"Lærerutdanninger og pedagogikk"},
    {code:3,label:"Samfunnsfag og juridiske fag"},
    {code:4,label:"Økonomiske og administrative fag"},
    {code:5,label:"Naturvitenskapelige fag, håndverksfag og tekniske fag"},
    {code:6,label:"Helse-, sosial- og idrettsfag"},
    {code:7,label:"Primærnæringsfag"},
    {code:8,label:"Samferdsel, sikkerhet og servicefag"},
    {code:9,label:"Uoppgitt"}
  ],

  // ---- Occupations: curated set with STYRK first digit + median monthly NOK ----
  // Sourced from SSB 11418 medianlønn 2025. styrk1 used for edu→occ mapping.
  occupations: [
    // 1: Ledere
    {label:"Administrerende direktør",styrk1:1,med:84000,hc:39086},
    {label:"Finans- og økonomisjef",styrk1:1,med:101670,hc:9109},
    {label:"Personalsjef",styrk1:1,med:96670,hc:2350},
    {label:"Salgs- og markedssjef",styrk1:1,med:93500,hc:8400},
    {label:"IT-sjef",styrk1:1,med:99800,hc:4200},
    {label:"Politiker",styrk1:1,med:88680,hc:2510},
    // 2: Akademiske yrker
    {label:"Lege",styrk1:2,med:84500,hc:25600},
    {label:"Tannlege",styrk1:2,med:78200,hc:5800},
    {label:"Psykolog",styrk1:2,med:62500,hc:11200},
    {label:"Sivilingeniør (bygg)",styrk1:2,med:75200,hc:18400},
    {label:"Sivilingeniør (data)",styrk1:2,med:79800,hc:22300},
    {label:"Sivilingeniør (petroleum)",styrk1:2,med:101400,hc:6800},
    {label:"Forsker (universitet)",styrk1:2,med:64500,hc:9200},
    {label:"Førsteamanuensis",styrk1:2,med:71200,hc:6800},
    {label:"Advokat",styrk1:2,med:78900,hc:8400},
    {label:"Statsadvokat",styrk1:2,med:88500,hc:600},
    {label:"Lektor",styrk1:2,med:62300,hc:18900},
    {label:"Arkitekt",styrk1:2,med:64800,hc:5400},
    {label:"Journalist",styrk1:2,med:58400,hc:7600},
    {label:"Forfatter",styrk1:2,med:42800,hc:1800},
    {label:"Prest (Den norske kirke)",styrk1:2,med:62100,hc:2400},
    // 3: Høyskoleyrker
    {label:"Sykepleier",styrk1:3,med:54300,hc:96400},
    {label:"Spesialsykepleier",styrk1:3,med:60800,hc:18200},
    {label:"Jordmor",styrk1:3,med:62400,hc:2900},
    {label:"Fysioterapeut",styrk1:3,med:56200,hc:11800},
    {label:"Bioingeniør",styrk1:3,med:55700,hc:6800},
    {label:"Politibetjent",styrk1:3,med:62800,hc:9400},
    {label:"Brannkonstabel",styrk1:3,med:55400,hc:3800},
    {label:"Lærer i grunnskolen",styrk1:3,med:56100,hc:78400},
    {label:"Barnehagelærer",styrk1:3,med:51200,hc:32800},
    {label:"Ingeniør (bygg)",styrk1:3,med:65400,hc:14800},
    {label:"Ingeniør (elektro)",styrk1:3,med:66800,hc:11200},
    {label:"Tekniker (data)",styrk1:3,med:58400,hc:14600},
    {label:"Sosionom",styrk1:3,med:51800,hc:8900},
    {label:"Vernepleier",styrk1:3,med:53400,hc:14200},
    // 4: Kontor
    {label:"Saksbehandler i offentlig sektor",styrk1:4,med:54200,hc:44800},
    {label:"Lønnsmedarbeider",styrk1:4,med:51400,hc:8400},
    {label:"Resepsjonist",styrk1:4,med:42800,hc:7800},
    {label:"Bibliotekar",styrk1:4,med:49600,hc:3200},
    // 5: Salg / service
    {label:"Butikkmedarbeider",styrk1:5,med:38400,hc:122000},
    {label:"Servitør",styrk1:5,med:36800,hc:24400},
    {label:"Kokk",styrk1:5,med:42600,hc:18800},
    {label:"Frisør",styrk1:5,med:39200,hc:7400},
    {label:"Hjelpepleier",styrk1:5,med:46100,hc:74600},
    {label:"Helsefagarbeider",styrk1:5,med:46800,hc:62100},
    {label:"Barnehageassistent",styrk1:5,med:39800,hc:42600},
    {label:"Sikkerhetsvakt",styrk1:5,med:43200,hc:8800},
    // 6: Bønder / fiskere
    {label:"Bonde (jordbruk)",styrk1:6,med:38400,hc:14800},
    {label:"Fisker",styrk1:6,med:54200,hc:7800},
    {label:"Reineier",styrk1:6,med:32400,hc:480},
    {label:"Skogbruker",styrk1:6,med:42800,hc:1400},
    // 7: Håndverkere
    {label:"Tømrer",styrk1:7,med:48200,hc:38400},
    {label:"Elektriker",styrk1:7,med:51800,hc:24600},
    {label:"Rørlegger",styrk1:7,med:50400,hc:14200},
    {label:"Murer",styrk1:7,med:48700,hc:6800},
    {label:"Maler",styrk1:7,med:43400,hc:8400},
    {label:"Bilmekaniker",styrk1:7,med:46200,hc:14800},
    {label:"Sveiser",styrk1:7,med:49800,hc:8200},
    {label:"Snekker",styrk1:7,med:46800,hc:11400},
    // 8: Prosess / maskin / sjåfør
    {label:"Lastebilsjåfør",styrk1:8,med:46800,hc:32400},
    {label:"Bussjåfør",styrk1:8,med:44200,hc:14600},
    {label:"Drosjesjåfør",styrk1:8,med:38600,hc:5800},
    {label:"Anleggsmaskinfører",styrk1:8,med:48200,hc:11800},
    {label:"Skipsfører",styrk1:8,med:78400,hc:6800},
    {label:"Pilot",styrk1:8,med:91400,hc:2900},
    // 9: Renhold / hjelp
    {label:"Renholder",styrk1:9,med:38200,hc:42800},
    {label:"Lager- og logistikkmedarbeider",styrk1:9,med:42600,hc:38400},
    {label:"Avisbud",styrk1:9,med:32400,hc:1800}
  ],

  // ---- Household types ----
  households: [
    {code:1,label:"Aleneboende",share:0.185},
    {code:2,label:"Par uten barn",share:0.200},
    {code:3,label:"Par med barn 0–5 år",share:0.105},
    {code:4,label:"Par med barn 6–17 år",share:0.150},
    {code:5,label:"Par med voksne barn",share:0.080},
    {code:6,label:"Enslig med barn 0–17 år",share:0.065},
    {code:7,label:"Enslig med voksne barn",share:0.045},
    {code:8,label:"Flerfamiliehusholdning",share:0.040},
    {code:9,label:"Bofellesskap",share:0.030},
    {code:10,label:"Annen husholdning",share:0.025},
    {code:11,label:"Bor hos foreldre",share:0.075}
  ],

  // ---- Marital status (18+) ----
  maritalStatus: [
    {code:1,label:"Ugift",share:0.395},
    {code:2,label:"Gift",share:0.370},
    {code:3,label:"Enke / enkemann",share:0.040},
    {code:4,label:"Skilt",share:0.105},
    {code:5,label:"Separert",share:0.015},
    {code:6,label:"Registrert partner",share:0.005},
    {code:7,label:"Samboer",share:0.070}
  ],

  // ---- Religion (curated baseline, with codes for humor lookup) ----
  religions: [
    {code:"DnK", label:"Den norske kirke",share:0.66},
    {code:"INGEN", label:"Ikke-religiøs / Human-Etisk Forbund",share:0.18},
    {code:"KAT", label:"Den katolske kirke",share:0.04},
    {code:"ISL", label:"Islam",share:0.035},
    {code:"ANN_KRIS", label:"Andre kristne trossamfunn",share:0.04},
    {code:"HUM", label:"Human-Etisk Forbund",share:0.005},
    {code:"BUD", label:"Buddhisme",share:0.005},
    {code:"HIN", label:"Hinduisme",share:0.003},
    {code:"JOD", label:"Jødedom",share:0.001},
    {code:"ANN", label:"Annet / uoppgitt",share:0.031}
  ],

  // ---- Parties (national baseline 2025, with codes for humor lookup) ----
  parties: [
    {code:"AP",  label:"Arbeiderpartiet",share:0.245},
    {code:"H",   label:"Høyre",share:0.220},
    {code:"FRP", label:"Fremskrittspartiet",share:0.165},
    {code:"SP",  label:"Senterpartiet",share:0.080},
    {code:"SV",  label:"Sosialistisk Venstreparti",share:0.080},
    {code:"R",   label:"Rødt",share:0.060},
    {code:"V",   label:"Venstre",share:0.040},
    {code:"KRF", label:"Kristelig Folkeparti",share:0.035},
    {code:"MDG", label:"Miljøpartiet De Grønne",share:0.040},
    {code:"ANDRE", label:"Andre",share:0.020},
    {code:"STEMTE_IKKE", label:"Stemte ikke",share:0.015}
  ],

  // ---- Pensioner labels (67+) ----
  pensjonist: [
    "Hyttepusser","Turlag-general","Kaffeslabberas-koordinator",
    "Strikkedronning","Strikkekonge","Kryssordmester",
    "Barnebarn-logistiker","Bridgeekspert","Hageentusiast",
    "Frimerkesamler","Sudokumester","Turgåer","Fjordfisker",
    "Vedhogger","Kirkekorsanger","Baketeoretiker","Lotto-veteran",
    "Kaffekos-ambassadør"
  ],
  // ---- Kids ----
  toddler: ["Saftsommelier","Sandkassesjef","Lekeklosspilot","Dinosaurekspert","Puslespillmester","Kosedyrsjef","Trehjulsyklist","Vannpytt-inspektør"],
  school: ["Minecraft-arkitekt","Fotballentusiast","Pokémonsamler","Slime-produsent","Trampolineakrobat","Lego-ingeniør","Klassens klovn","Sykkelstuntmann","SFO-veteran"],
  teen: ["TikTok-koreograf","Discord-moderator","Fortnite-strateg","Snap-streak-vokter","Russebuss-spekulant","Skatepark-stamgjest","Spotify-playlistekurator"],
  uppersec: ["Russeknuter","Kassa-veteran","Kino-billettselger","Barista-in-training","Gymnasiast","Lærling","Festival-frivillig","Eksamensangst-kjemper"],

  // ---- Income deciles ----
  incomeDeciles: [
    {d:1,lo:0,hi:150000,label:"Under 150 000 kr"},
    {d:2,lo:150000,hi:270000,label:"150 000 – 270 000 kr"},
    {d:3,lo:270000,hi:340000,label:"270 000 – 340 000 kr"},
    {d:4,lo:340000,hi:400000,label:"340 000 – 400 000 kr"},
    {d:5,lo:400000,hi:460000,label:"400 000 – 460 000 kr"},
    {d:6,lo:460000,hi:530000,label:"460 000 – 530 000 kr"},
    {d:7,lo:530000,hi:610000,label:"530 000 – 610 000 kr"},
    {d:8,lo:610000,hi:720000,label:"610 000 – 720 000 kr"},
    {d:9,lo:720000,hi:900000,label:"720 000 – 900 000 kr"},
    {d:10,lo:900000,hi:2000000,label:"Over 900 000 kr"}
  ],

  // ---- Wealth deciles (rough Norwegian, 2024 individual netto) ----
  wealthClasses: [
    {label:"Negativ formue (gjeld > eiendeler)", lo:-1500000, hi:0, share:0.18},
    {label:"Lav formue", lo:0, hi:300000, share:0.22},
    {label:"Middels formue", lo:300000, hi:1500000, share:0.30},
    {label:"Solid formue", lo:1500000, hi:5000000, share:0.20},
    {label:"Velstående", lo:5000000, hi:20000000, share:0.08},
    {label:"Svært velstående", lo:20000000, hi:100000000, share:0.018},
    {label:"Topp 1 %", lo:100000000, hi:500000000, share:0.0018},
    {label:"Topp 0,1 %", lo:500000000, hi:5000000000, share:0.0002}
  ],

  // ---- Sivilstand-jokere ----
  jokesAdult: ["Forlovet med en badeand","Har forlatt sin mann for en traktor","Gift med jobben (bokstavelig talt)","Separert fra virkeligheten","I et langdistanseforhold med kontoen sin"],
  jokesElder: ["Gift med kaffen sin siden 1973","Enke etter en svært dyr båt","Forlovet med kryssordene i Aftenposten","I et åpent forhold med hytteboka"],

  // ---- Innvandrerbakgrunn (SSB 09817 ~2024) ----
  // ~16 % førstegen + 4 % andregen ≈ 20 % av befolkningen.
  // Resten: norskfødt med to norskfødte foreldre.
  origins: [
    // Top opphavsland med samlet 1.+2.gen-vekt og innvandringsregion
    {code:131, label:"Polen",            region:"ost_europa",     w:129772},
    {code:148, label:"Ukraina",          region:"ost_europa",     w:88235},
    {code:564, label:"Syria",            region:"mena_sor_asia",  w:50668},
    {code:136, label:"Litauen",          region:"ost_europa",     w:51467},
    {code:346, label:"Somalia",          region:"afrika_sub",     w:44744},
    {code:534, label:"Pakistan",         region:"mena_sor_asia",  w:44113},
    {code:106, label:"Sverige",          region:"norden",         w:41606},
    {code:452, label:"Irak",             region:"mena_sor_asia",  w:36991},
    {code:241, label:"Eritrea",          region:"afrika_sub",     w:36307},
    {code:144, label:"Tyskland",         region:"vesteuropa",     w:32051},
    {code:428, label:"Filippinene",      region:"latam_filippin", w:29913},
    {code:404, label:"Afghanistan",      region:"mena_sor_asia",  w:27871},
    {code:140, label:"Russland",         region:"ost_europa",     w:27475},
    {code:456, label:"Iran",             region:"mena_sor_asia",  w:26497},
    {code:444, label:"India",            region:"mena_sor_asia",  w:25016},
    {code:568, label:"Thailand",         region:"ost_asia",       w:24722},
    {code:575, label:"Vietnam",          region:"ost_asia",       w:24530},
    {code:143, label:"Tyrkia",           region:"mena_sor_asia",  w:24350},
    {code:133, label:"Romania",          region:"ost_europa",     w:22240},
    {code:101, label:"Danmark",          region:"norden",         w:20168},
    {code:155, label:"Bosnia-Hercegovina",region:"ost_europa",    w:19203},
    {code:139, label:"Storbritannia",    region:"vesteuropa",     w:18773},
    {code:161, label:"Kosovo",           region:"ost_europa",     w:17978},
    {code:684, label:"USA",              region:"vesteuropa",     w:13500},
    {code:578, label:"Etiopia",          region:"afrika_sub",     w:11200},
    {code:111, label:"Hellas",           region:"vesteuropa",     w:5800},
    {code:151, label:"Kina",             region:"ost_asia",       w:14200},
    {code:160, label:"Sør-Korea",        region:"ost_asia",       w:2400},
    {code:412, label:"Brasil",           region:"latam_filippin", w:5800}
  ],
  // P(immigrant background) ≈ 20 %. Of which ~80 % førstegen, 20 % andregen.
  immigrantShare: 0.20,
  secondGenShare: 0.20,

  // ---- Names by region (slim, for immigrant naming) ----
  namesByRegion: {
    norden:        {M:["Karl","Lars","Anders","Erik","Magnus","Henrik","Per","Sven","Johan","Anton","Oskar","Mikkel"],
                    F:["Anna","Astrid","Karin","Linnea","Maja","Elin","Ida","Stina","Tora","Sigrid"]},
    vesteuropa:    {M:["Hans","Klaus","Friedrich","James","David","Michael","Pierre","Marco","Diego","Luca"],
                    F:["Hannah","Sophie","Emma","Charlotte","Marie","Anna","Maria","Isabel","Giulia"]},
    ost_europa:    {M:["Jakub","Tomasz","Piotr","Andrzej","Mykhailo","Oleksandr","Vladimir","Dmitri","Tomaš","Matej"],
                    F:["Katarzyna","Magdalena","Anna","Olena","Iryna","Natalya","Marija","Petra"]},
    mena_sor_asia: {M:["Mohammad","Ahmed","Ali","Hassan","Yusuf","Omar","Ibrahim","Reza","Arjun","Rahim"],
                    F:["Fatima","Aisha","Maryam","Zara","Layla","Noor","Farah","Priya"]},
    afrika_sub:    {M:["Abdi","Hassan","Mohamed","Yonas","Daniel","Samuel","Yohannes","Tesfay"],
                    F:["Amina","Fartun","Hawa","Saba","Senait","Hanna","Genet"]},
    ost_asia:      {M:["Wei","Hao","Long","Minh","Thanh","Somchai","Niran","Kim"],
                    F:["Xia","Mei","Linh","Hoa","Mai","Nok","Ploy","Sun-hi"]},
    latam_filippin:{M:["Carlos","Juan","Diego","Miguel","Jose","Mark","Ramon","Luis"],
                    F:["Maria","Ana","Rosa","Carmen","Isabel","Angel","Lorena"]}
  },

  // ---- Religion by origin region (conditional probs, for immigrants) ----
  religionByRegion: {
    norden:        {DnK:0.50, KAT:0.04, ANN_KRIS:0.05, ISL:0.01,  HUM:0.04,  BUD:0.005, HIN:0.005, JOD:0.0005, ANN:0.02,  INGEN:0.339},
    vesteuropa:    {DnK:0.05, KAT:0.13, ANN_KRIS:0.20, ISL:0.04,  HUM:0.04,  BUD:0.01,  HIN:0.01,  JOD:0.001,  ANN:0.045, INGEN:0.420},
    ost_europa:    {DnK:0.02, KAT:0.22, ANN_KRIS:0.32, ISL:0.06,  HUM:0.02,  BUD:0.005, HIN:0.005, JOD:0.001,  ANN:0.030, INGEN:0.319},
    mena_sor_asia: {DnK:0.005,KAT:0.005,ANN_KRIS:0.03, ISL:0.78,  HUM:0.005, BUD:0.04,  HIN:0.08,  JOD:0.001,  ANN:0.020, INGEN:0.029},
    afrika_sub:    {DnK:0.01, KAT:0.04, ANN_KRIS:0.30, ISL:0.55,  HUM:0.005, BUD:0.005, HIN:0.005, JOD:0.0005, ANN:0.060, INGEN:0.025},
    ost_asia:      {DnK:0.005,KAT:0.02, ANN_KRIS:0.04, ISL:0.02,  HUM:0.005, BUD:0.40,  HIN:0.005, JOD:0.0005, ANN:0.20,  INGEN:0.305},
    latam_filippin:{DnK:0.02, KAT:0.40, ANN_KRIS:0.20, ISL:0.03,  HUM:0.01,  BUD:0.005, HIN:0.005, JOD:0.001,  ANN:0.07,  INGEN:0.259}
  },

  // ---- Bolig: gjennomsnittlig boligpris per fylke 2025 (kr/kvm, alle boligtyper) ----
  // Kalibrert grovt mot SSB 03365. Multipliseres med ~80 m² for typisk leilighet.
  housingPricePerSqm: {
    "Oslo": 86000, "Akershus": 64000, "Vestland": 55000, "Rogaland": 48000,
    "Trøndelag": 47000, "Vestfold": 41000, "Buskerud": 42000, "Agder": 38000,
    "Østfold": 36000, "Innlandet": 32000, "Møre og Romsdal": 35000,
    "Nordland": 30000, "Telemark": 32000, "Troms": 39000, "Finnmark": 28000
  },
  // Boligprisindex (2015 = 100), grovt fra SSB. Brukes for å regne historiske kjøpspriser.
  // år -> indeks for hele landet, alle boligtyper.
  housingIndex: {
    1995:18, 2000:30, 2005:53, 2010:78, 2015:100, 2020:121, 2024:140, 2025:142
  },
  // P(eier bolig) etter aldersgruppe (grovt, SSB 06265).
  ownerByAge: [
    [0,17, 0.00], [18,24, 0.18], [25,29, 0.42], [30,34, 0.65],
    [35,44, 0.79], [45,54, 0.83], [55,66, 0.84], [67,74, 0.83], [75,99, 0.78]
  ],

  // ---- Party humor labels (CounterfactMe party_humor.csv) ----
  partyHumor: {
    AP:["Stemmer det moren stemte — og hun stemte AP","Vokste opp på Furuset","Stemmer AP fordi LO sa det","Sier 'sosialdemokrati' som om det er et uttrykk","Synes Støre er kjedelig men trygg","Faner i 1. mai-toget — hjem igjen klokka 14","Aktiv i fagforeningen — har skrevet leserinnlegg"],
    H:["Stemmer H — drikker hvitvin og ser Bjelland","Hører på Civita-podcast på vei til hytta","Mener seg sentrum-orientert — er det ikke","Stemmer H fordi det er smart for skatten","Eier en Tesla og to leiligheter — derfor H","Var med i UH før Erna ble noen"],
    FRP:["Synes Sylvi sier det andre tenker","Vil ha lavere bilavgifter — koste hva det vil","Skifta fra Ap til Frp i 2013 og angrer ikke","Kommenterer høylytt på Facebook","Mener Norge bygges nedenfra og opp","Stemte Frp som ung og stemmer Frp som gammel"],
    SV:["Stemmer SV men gråter litt over Audun","Tatovering av Marx — ler av seg selv","Boikotter Israel — glemmer kibbutz-onkelen","Var medlem av SU på Blindern","Sender Lyspæren til Svalbard hvert år"],
    SP:["Driver gård og synes det er greit","Stemmer Sp fordi byrokratiet i Oslo er irriterende","Mener Vedum er en fin fyr","Pendler til kommunesentrum — hjem til hytta i helgene","Hatkjærlighet til EØS"],
    V:["Stemmer V som en livsstil","Drikker mate-te, leser Klassekampen, stemmer V","Var med på 'jeg-er-grønn'-marsjen","Kjente Trine fra studenttiden","Sykler til jobb — kjører Audi til hytta"],
    KRF:["Foreldrene gjorde det og det funker","Misjon, alkohol-skepsis, KrF","Stemmer KrF fordi Hareide var fin","Møter i bedehuset hver søndag","Skuffa over abortliberaliseringen — holder fast"],
    R:["Synes Moxnes er kjekk","Stemmer R, jobber i Coop, ler av kontradiksjonen","Bærer aldri Mao-merke — har sett alle dokumentarene","Var med i RU på 90-tallet","Vil ha makspris på leie — eier selv tre hybler","Sier 'kapitalismen' som om det er en personlig fornærmelse"],
    MDG:["Sykler hele året og skiller på melk","Stemmer MDG — kjører diesel-Audi","Vegan tre dager i uka","Trodde Lan Marie var statsråd — ble skuffet","Hater bensinprisen mer enn klimaendringer"],
    ANDRE:["Stemte INP, siterer Kringkastingsrådet","Stemmer Demokratene, vet at det er bortkastet","Liberalisten — har lest Mises på engelsk","Pasientfokus — engasjert etter pårørende-erfaring","Egen liste — 'Innbyggernes parti for ekte fornuft'"],
    STEMTE_IKKE:["Synes Moxnes er kjekk, men var for fyllesyk til å stemme i år","Stemmer aldri på prinsipp","Glemte å registrere flytting","Skiftet mening i stemmeavlukket og stemte blank","Tilhører de stille flertallet — har ikke stemt siden 1997","Var ute på hytta — glemte å forhåndsstemme","Mener at alle politikere er like dårlige","Glemte at det var valg"]
  },

  // ---- Religion humor labels (CounterfactMe religion_humor.csv) ----
  religionHumor: {
    DnK:["Konfirmert i 1986, ellers usynlig medlem","Sier 'jeg er ikke religiøs men medlem av kirka'","Stiller opp i kirka kun til jul og 17. mai","Dåp, konfirmasjon, bryllup, begravelse — fullpakka kunde","Mente å melde seg ut etter Brennpunkt, glemte det","Sender 100-lapp til Bymisjonen i adventstida og tenker det får telle","Klikket aldri 'meld meg ut' på Den norske kirke-sida","Stille DnK-medlem som alle andre i bygda"],
    KAT:["Polsk arv, har rosenkrans i baklomma","Konvertert i 2014, går sjelden i messe","Mor er polsk, derfor er jeg det også","Tar Maria-andakt på telefon hver kveld før hun sovner","Bekjenner litt for ofte — presten begynner å se sliten ut"],
    ANN_KRIS:["Pinsemenighet — taler i tunger på menighetsweekenden","Adventist — gjør ingenting på lørdager","Jehovas vitne — banker på dører om lørdagen","Frikirka, full pakke","Ortodoks (russisk), feirer julen 7. januar","Eritreisk-ortodoks, går i kirken på Furuset"],
    ISL:["Praktiserende, ber fem ganger om dagen","Faster i ramadan, drikker kaffe ellers","Kulturell muslim, spiser bacon i smug","Halal hjemme, pølse på Narvesen i lunsjen","Eid-feiring som høydepunkt — resten av året vanlig livet","Bærer hijab på fest, ikke på jobb (eller motsatt)"],
    HUM:["Konfirmert humanist og ganske fornøyd","Sender penger til Human-Etisk hver november","Argumenterer mot DnK på Facebook hver desember","Bestilt Vampus-mug fra Human-Etisk og har den på kontoret","Mener bestemt vitenskap er svaret, men jamrer over kvantefysikken"],
    BUD:["Mediterer 20 min hver morgen — mister det rundt mai","Liker å ha Buddha-statuer i hagen","Yoga-lærer som sier 'namaste' før første kaffe","Vipassana-retreat hver høst — snakker om det resten av året","Fant fred på et retreat på Koh Samui i 2018"],
    HIN:["Tamiltradisjon, tenner lysene på Diwali","Hindu i navnet, agnostiker i praksis","Pilegrimsreise til Varanasi i 2019 — fortsatt fortalt om","Møter på templet i Drammen til Holi, ellers nei"],
    JOD:["Liten norsk-jødisk familie, holder pesach hvert år","Praktiserende konservativ — aktiv i Mosaisk Trossamfunn","Holder shabbat når foreldrene besøker, ikke ellers"],
    ANN:["Pastafarianer — bærer pastasil i passbildet","Åsatru — feirer blot på vinteren","Praktiserende wicca","Følger en Substack om stoisisme og mener seg religiøs","Bahai — har vært på Lotus-tempelet i Delhi, viser bilder","Identifiserer seg som 'spirituell ikke religiøs'","Scientolog (sier det ikke høyt på jobb)"],
    INGEN:["Var konfirmant, kalla seg ateist resten av livet","'Tror på noe der ute' og lar det være med det","Tror på krystaller og månefaser","Sterk sekulær — kjenner seg igjen i Bjørn Eidsvåg-tekster","Meldte seg ut på prinsipp og fikk skattelette","Aldri vært døpt — foreldrene var Frp-tilhengere","Mener Richard Dawkins er litt for mye","Antitheist på X — atheist i bursdager","Spiritualist på Instagram, agnostiker i virkeligheten","Tror på vitenskap og litt mindfulness"]
  },

  // ---- v0.8.4 additions ----

  // ---- Self-rated health by age band (FHI levekårsundersøkelsen) ----
  selfRatedHealth: [
    {band:"0-15", probs:[0.65,0.30,0.04,0.01,0.00]},
    {band:"16-24", probs:[0.55,0.36,0.07,0.02,0.00]},
    {band:"25-44", probs:[0.40,0.45,0.11,0.03,0.01]},
    {band:"45-64", probs:[0.25,0.45,0.20,0.07,0.03]},
    {band:"65-79", probs:[0.15,0.40,0.30,0.10,0.05]},
    {band:"80+",   probs:[0.08,0.30,0.35,0.18,0.09]}
  ],
  srhLabels: ["Meget god","God","Så som så","Dårlig","Meget dårlig"],

  // ---- Chronic illness probability by age band (FHI) ----
  chronicIllness: [
    {band:"0-15", p:0.05},
    {band:"16-24", p:0.10},
    {band:"25-44", p:0.18},
    {band:"45-64", p:0.36},
    {band:"65-79", p:0.55},
    {band:"80+",   p:0.72}
  ],
  chronicTypes: [
    "Hjerte-/karsykdom","Diabetes","Astma/KOLS","Revmatisme/leddsykdom",
    "Psykisk lidelse","Kreft (tidligere)","Annen kronisk lidelse"
  ],

  // ---- Social isolation / loneliness (FHI + ESS) ----
  socialIsolation: [
    {band:"0-15", probs:[0.05,0.15,0.40,0.40]},
    {band:"16-24", probs:[0.18,0.32,0.32,0.18]},
    {band:"25-44", probs:[0.10,0.25,0.40,0.25]},
    {band:"45-64", probs:[0.08,0.22,0.42,0.28]},
    {band:"65-79", probs:[0.10,0.25,0.42,0.23]},
    {band:"80+",   probs:[0.20,0.30,0.32,0.18]}
  ],
  lonelinessLabels: [
    "Føler seg ofte ensom","Føler seg av og til ensom",
    "Føler seg sjelden ensom","Føler seg aldri ensom"
  ],

  // ---- Material deprivation by income decile (EU-SILC) ----
  // probs: [0 items missing, 1, 2, 3, 4+ items missing of 7 standard items]
  materialDeprivation: [
    {decile:1, probs:[0.40,0.20,0.15,0.10,0.15]},
    {decile:2, probs:[0.55,0.20,0.10,0.08,0.07]},
    {decile:3, probs:[0.65,0.18,0.08,0.05,0.04]},
    {decile:4, probs:[0.75,0.13,0.06,0.04,0.02]},
    {decile:5, probs:[0.82,0.10,0.04,0.03,0.01]},
    {decile:6, probs:[0.87,0.08,0.03,0.01,0.01]},
    {decile:7, probs:[0.91,0.06,0.02,0.01,0.00]},
    {decile:8, probs:[0.94,0.04,0.01,0.01,0.00]},
    {decile:9, probs:[0.97,0.02,0.01,0.00,0.00]},
    {decile:10,probs:[0.99,0.01,0.00,0.00,0.00]}
  ],

  // ---- Children per cohort (mother's birth year) ----
  nChildrenByCohort: [
    {min:1920,max:1940,probs:[0.07,0.13,0.30,0.27,0.23]},
    {min:1940,max:1955,probs:[0.09,0.13,0.36,0.27,0.15]},
    {min:1955,max:1965,probs:[0.13,0.15,0.40,0.22,0.10]},
    {min:1965,max:1975,probs:[0.16,0.17,0.42,0.18,0.07]},
    {min:1975,max:1985,probs:[0.19,0.18,0.42,0.16,0.05]},
    {min:1985,max:1995,probs:[0.23,0.21,0.38,0.14,0.04]},
    {min:1995,max:2010,probs:[0.45,0.20,0.25,0.08,0.02]}
  ],

  // ---- Bourdieu klasseposisjon labels ----
  bourdieuKlasser: [
    "Etablert overklasse","Økonomisk elite","Kulturell elite",
    "Etablert middelklasse","Kulturell middelklasse","Økonomisk middelklasse",
    "Tradisjonell arbeiderklasse","Ny arbeiderklasse","Prekariat"
  ]
};
