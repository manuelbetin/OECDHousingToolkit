htk_generate_ctry_fiches = function(Rmdfile="skeleton.Rmd",country_code_list,path=NULL,quiet=F,path_data,path_paragraphs) {
  #' @title generate the country fiche for the selected country
  #' @description generate the country fiche for the selected country
  #' @param Rmdfile the name of the file (.Rmd) that is used as template
  #' @param country_code_list a vector of iso3 codes for the
  #' @param path name of the directory where to store the pdf output
  #' group of countries that you want to generate the fiches
  #' @return pdf document from rmardown skeleton
  #' @author Manuel Betin
  #' @export
  #'


  if(!is.null(path)){
    dir.create(path)
  }
  lapply(country_code_list,function(country_code){
   country_name=countrycode::countrycode(country_code,origin="iso3c",destination="country.name")
   country_name=ifelse(country_code=="KOR", "Korea", country_name)
   country_name=ifelse(country_code=="CZE", "Czech Republic", country_name)
   country_name=ifelse(country_code=="SVK", "Slovak Republic", country_name)
   country_name=ifelse(country_code=="TUR", "Türkiye", country_name)
   country_adj=get_adjective() %>% filter(Country==country_name) %>% dplyr::select(Adjectivals) %>% pull()
   country_adj=tolower(country_adj)
   if(length(country_adj)==0){
     country_adj=paste0(country_name,"'s")
   }

    rmarkdown::render(
      Rmdfile, params = list(
        ctry_code = country_code,
        ctry_name=country_name,
        ctry_adj=country_adj,
        path_data=path_data,
        path_paragraphs=path_paragraphs
      ),
      output_dir = path,
      output_file = ifelse(!is.null(path),paste0(path,"/", "housing-policy-", country_name, ".pdf"),paste0("CountryFiches-", country_name, ".pdf")),
      quiet = quiet
    )
  })

}


get_adjective=function(){
  #' @title dataframe with name of countries, adjective and demonyms
  #' @description provide the demonyms and adjective for all countries
  #' @return dataframe with three columns Country,Adjectivals,Demonyms
  #' @author Manuel Betin
  #'

  structure(list(
    `Country` = c("Abkhazia", "Afghanistan",
                  "Åland Islands", "Albania", "Algeria", "American Samoa", "Andorra",
                  "Angola", "Anguilla", "Antarctica[a]", "Antigua and Barbuda",
                  "Argentina", "Armenia", "Aruba", "Australia", "Austria", "Azerbaijan",
                  "Bahamas, The", "Bahrain", "Bangladesh", "Barbados", "Belarus",
                  "Belgium", "Belize", "Benin", "Bermuda", "Bhutan", "Bolivia, Plurinational State of",
                  "Bonaire", "Bosnia and Herzegovina", "Botswana", "Bouvet Island",
                  "Brazil", "British Indian Ocean Territory", "Brunei", "Bulgaria",
                  "Burkina Faso", "Burma[b]", "Burundi", "Cabo Verde[c]", "Cambodia",
                  "Cameroon", "Canada", "Cayman Islands", "Central African Republic",
                  "Chad", "Chile", "China, People's Republic of", "China, Republic of[d]",
                  "Christmas Island", "Cocos (Keeling) Islands", "Colombia", "Comoros",
                  "Congo, Democratic Republic of the", "Congo, Republic of the",
                  "Cook Islands", "Costa Rica", "Croatia", "Cuba", "Curaçao",
                  "Cyprus", "Czech Republic", "Denmark", "Djibouti", "Dominica",
                  "Dominican Republic", "East Timor[g]", "Ecuador", "Egypt", "El Salvador",
                  "England", "Equatorial Guinea", "Eritrea", "Estonia", "Eswatini (Swaziland)[h]",
                  "Ethiopia", "European Union[i]", "Falkland Islands", "Faroe Islands",
                  "Fiji", "Finland", "France", "French Guiana", "French Polynesia",
                  "French Southern Territories", "Gabon", "Gambia, The", "Georgia",
                  "Germany", "Ghana", "Gibraltar", "Greece", "Greenland", "Grenada",
                  "Guadeloupe", "Guam", "Guatemala", "Guernsey", "Guinea", "Guinea-Bissau",
                  "Guyana", "Haiti", "Heard Island and McDonald Islands", "Honduras",
                  "Hong Kong", "Hungary", "Iceland", "India", "Indonesia", "Iran, Islamic Republic of",
                  "Iraq", "Ireland[k]", "Isle of Man", "Israel", "Italy", "Ivory Coast[l]",
                  "Jamaica", "Jan Mayen", "Japan", "Jersey", "Jordan", "Kazakhstan",
                  "Kenya", "Kiribati", "Korea, Democratic People's Republic of",
                  "Korea, Republic of", "Kosovo", "Kuwait", "Kyrgyzstan", "Lao People's Democratic Republic",
                  "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein",
                  "Lithuania", "Luxembourg", "Macau", "Madagascar", "Malawi", "Malaysia",
                  "Maldives", "Mali", "Malta", "Marshall Islands", "Martinique",
                  "Mauritania", "Mauritius", "Mayotte", "Mexico", "Micronesia, Federated States of",
                  "Moldova", "Monaco", "Mongolia", "Montenegro", "Montserrat",
                  "Morocco", "Mozambique", "Myanmar[m]", "Namibia", "Nauru", "Nepal",
                  "Netherlands", "New Caledonia", "New Zealand", "Nicaragua", "Niger",
                  "Nigeria", "Niue", "Norfolk Island", "North Macedonia", "Northern Ireland",
                  "Northern Mariana Islands", "Norway", "Oman", "Pakistan", "Palau",
                  "Palestine", "Panama", "Papua New Guinea", "Paraguay", "Peru",
                  "Philippines", "Pitcairn Islands", "Poland", "Portugal", "Puerto Rico",
                  "Qatar", "Réunion", "Romania", "Russia[n]", "Rwanda", "Saba",
                  "Saint Barthélemy", "Saint Helena, Ascension and Tristan da Cunha",
                  "Saint Kitts and Nevis", "Saint Lucia", "Saint Martin", "Saint Pierre and Miquelon",
                  "Saint Vincent and the Grenadines", "Sahrawi Arab Democratic Republic",
                  "Samoa", "San Marino", "São Tomé and Príncipe", "Saudi Arabia",
                  "Scotland", "Senegal", "Serbia", "Seychelles", "Sierra Leone",
                  "Singapore", "Sint Eustatius", "Sint Maarten", "Slovakia", "Slovenia",
                  "Solomon Islands", "Somalia", "Somaliland", "South Africa", "South Georgia and the South Sandwich Islands",
                  "South Ossetia", "South Sudan", "Spain", "Sri Lanka", "Sudan",
                  "Suriname", "Svalbard", "Sweden", "Switzerland", "Syrian Arab Republic",
                  "Tajikistan", "Tanzania", "Thailand", "Timor-Leste[o]", "Togo",
                  "Tokelau", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey",
                  "Turkmenistan", "Turks and Caicos Islands", "Tuvalu", "Uganda",
                  "Ukraine", "United Arab Emirates", "United Kingdom of Great Britain and Northern Ireland",
                  "United States of America", "Uruguay", "Uzbekistan", "Vanuatu",
                  "Vatican City State", "Venezuela, Bolivarian Republic of", "Vietnam",
                  "Virgin Islands, British", "Virgin Islands, United States", "Wales",
                  "Wallis and Futuna", "Western Sahara", "Yemen", "Zambia", "Zimbabwe"
    ),
    Adjectivals = c("Abkhaz, Abkhazian", "Afghan", "Åland Island",
                    "Albanian", "Algerian", "American Samoan", "Andorran", "Angolan",
                    "Anguillan", "Antarctic", "Antiguan or Barbudan", "Argentine,[1]Argentinian[2]",
                    "Armenian", "Aruban", "Australian", "Austrian", "Azerbaijani, Azeri",
                    "Bahamian", "Bahraini", "Bangladeshi", "Barbadian", "Belarusian",
                    "Belgian", "Belizean", "Beninese, Beninois", "Bermudian, Bermudan",
                    "Bhutanese", "Bolivian", "Bonaire, Bonairean", "Bosnian or Herzegovinian",
                    "Motswana, Botswanan", "Bouvet Island", "Brazilian", "BIOT",
                    "Bruneian", "Bulgarian", "Burkinabé", "Burmese", "Burundian",
                    "Cabo Verdean", "Cambodian", "Cameroonian", "Canadian", "Caymanian",
                    "Central African", "Chadian", "Chilean", "Chinese", "Taiwanese,  Formosan",
                    "Christmas Island", "Cocos Island", "Colombian", "Comoran, Comorian",
                    "Congolese", "Congolese", "Cook Island", "Costa Rican", "Croatian",
                    "Cuban", "Curaçaoan", "Cypriot", "Czech", "Danish", "Djiboutian",
                    "Dominican[e]", "Dominican[f]", "Timorese", "Ecuadorian", "Egyptian",
                    "Salvadoran", "English people, English", "Equatorial Guinean, Equatoguinean",
                    "Eritrean", "Estonian", "Swazi, Swati", "Ethiopian", "European[j]",
                    "Falkland Island", "Faroese", "Fijian", "Finnish", "French",
                    "French Guianese", "French Polynesian", "French Southern Territories",
                    "Gabonese", "Gambian", "Georgian", "German", "Ghanaian", "Gibraltar",
                    "Greek, Hellenic", "Greenlandic", "Grenadian", "Guadeloupe",
                    "Guamanian", "Guatemalan", "Channel Island", "Guinean", "Bissau-Guinean",
                    "Guyanese", "Haitian", "Heard Island or McDonald Islands", "Honduran",
                    "Hong Kong, Cantonese, Hong Konger", "Hungarian, Magyar", "Icelandic",
                    "Indian", "Indonesian", "Iranian, Persian", "Iraqi", "Irish",
                    "Manx", "Israeli, Israelite", "Italian", "Ivorian", "Jamaican",
                    "Jan Mayen", "Japanese", "Channel Island", "Jordanian", "Kazakhstani, Kazakh",
                    "Kenyan", "I-Kiribati", "North Korean", "South Korean", "Kosovar, Kosovan",
                    "Kuwaiti", "Kyrgyzstani, Kyrgyz, Kirgiz, Kirghiz", "Lao, Laotian",
                    "Latvian, Lettish", "Lebanese", "Basotho", "Liberian", "Libyan",
                    "Liechtensteiner", "Lithuanian", "Luxembourg, Luxembourgish",
                    "Macanese", "Malagasy", "Malawian", "Malaysian", "Maldivian",
                    "Malian, Malinese", "Maltese", "Marshallese", "Martiniquais, Martinican",
                    "Mauritanian", "Mauritian", "Mahoran", "Mexican", "Micronesian",
                    "Moldovan", "Monégasque, Monacan", "Mongolian", "Montenegrin",
                    "Montserratian", "Moroccan", "Mozambican", "Myanma or Burmese",
                    "Namibian", "Nauruan", "Nepali, Nepalese", "Dutch, Netherlandic",
                    "New Caledonian", "New Zealand", "Nicaraguan", "Nigerien", "Nigerian",
                    "Niuean", "Norfolk Island", "Macedonian", "Northern Irish", "Northern Marianan",
                    "Norwegian", "Omani", "Pakistani", "Palauan", "Palestinian",
                    "Panamanian", "Papua New Guinean, Papuan", "Paraguayan", "Peruvian",
                    "Filipino, Philippine", "Pitcairn Island", "Polish", "Portuguese",
                    "Puerto Rican", "Qatari", "Réunionese, Réunionnais", "Romanian",
                    "Russian", "Rwandan", "Saba, Saban", "Barthélemois", "Saint Helenian",
                    "Kittitian or Nevisian", "Saint Lucian", "Saint-Martinoise",
                    "Saint-Pierrais, Miquelonnais", "Saint Vincentian, Vincentian",
                    "Sahrawi, Western Saharan, Sahrawian", "Samoan", "Sammarinese",
                    "São Toméan", "Saudi, Saudi Arabian", "Scottish", "Senegalese",
                    "Serbian", "Seychellois", "Sierra Leonean", "Singapore, Singaporean[3][4]",
                    "Sint Eustatius, Statian", "Sint Maarten", "Slovak", "Slovenian, Slovene",
                    "Solomon Island", "Somali", "Somalilander", "South African",
                    "South Georgia Island, South Sandwich Island", "South Ossetian",
                    "South Sudanese", "Spanish", "Sri Lankan", "Sudanese", "Surinamese",
                    "Svalbard resident", "Swedish", "Swiss", "Syrian", "Tajikistani",
                    "Tanzanian", "Thai", "Timorese", "Togolese", "Tokelauan", "Tongan",
                    "Trinidadian or Tobagonian", "Tunisian", "Turkish", "Turkmen",
                    "Turks and Caicos Island", "Tuvaluan", "Ugandan", "Ukrainian",
                    "Emirati, Emirian, Emiri", "British, United Kingdom, UK", "American,[p]United States,  U.S.",
                    "Uruguayan", "Uzbekistani, Uzbek", "Ni-Vanuatu, Vanuatuan", "Vatican",
                    "Venezuelan", "Vietnamese", "British Virgin Island", "U.S. Virgin Island",
                    "Welsh", "Wallis and Futuna, Wallisian, Futunan", "Sahrawi, Sahrawian, Sahraouian",
                    "Yemeni", "Zambian", "Zimbabwean"),
    Demonyms = c("Abkhazians",
                 "Afghans", "Åland Islanders", "Albanians", "Algerians", "American Samoans",
                 "Andorrans", "Angolans", "Anguillans", "Antarctic residents",
                 "Antiguans or Barbudans", "Argentines, Argentinians", "Armenians",
                 "Arubans", "Australians", "Austrians", "Azerbaijanis, Azeris",
                 "Bahamians", "Bahrainis", "Bangladeshis", "Barbadians", "Belarusians",
                 "Belgians", "Belizeans", "Beninese, Beninois", "Bermudians, Bermudans",
                 "Bhutanese", "Bolivians", "Bonaire Dutch", "Bosnians or Herzegovinians",
                 "Batswana (singular Motswana), Botswanans", "Bouvet Islanders",
                 "Brazilians", "British", "Bruneians", "Bulgarians", "Burkinabè/Burkinabé",
                 "Burmese, Burman", "Burundians, Barundi", "Cabo Verdeans", "Cambodians",
                 "Cameroonians", "Canadians", "Caymanians", "Central Africans",
                 "Chadians", "Chileans", "Chinese", "Taiwanese,  Formosan", "Christmas Islanders",
                 "Cocos Islanders", "Colombians", "Comorans, Comorians", "Congolese",
                 "Congolese", "Cook Islanders", "Costa Ricans", "Croatians, Croats",
                 "Cubans", "Curaçaoans", "Cypriots", "Czechs", "Danes", "Djiboutians",
                 "Dominicans[e]", "Dominicans[f]", "Timorese", "Ecuadorians",
                 "Egyptians", "Salvadorans", "English, Englishmen/Englishwomen",
                 "Equatorial Guineans, Equatoguineans", "Eritreans", "Estonians",
                 "Swazis", "Ethiopians, Habesha", "Europeans[j]", "Falkland Islanders",
                 "Faroese", "Fijians", "Finns", "French, Frenchmen/Frenchwomen",
                 "French Guianese", "French Polynesians", "French", "Gabonese, Gabonaise",
                 "Gambians", "Georgians", "Germans", "Ghanaians", "Gibraltarians",
                 "Greeks, Hellenes", "Greenlanders", "Grenadians", "Guadeloupians, Guadeloupeans",
                 "Guamanians", "Guatemalans", "Channel Islanders", "Guineans",
                 "Bissau-Guineans", "Guyanese", "Haitians", "Heard Islanders or McDonald Islander",
                 "Hondurans", "Hongkongers, Hong Kongese", "Hungarians, Magyars",
                 "Icelanders", "Indians", "Indonesians", "Iranians, Persians",
                 "Iraqis", "Irish, Irishmen/Irishwomen", "Manx", "Israelis", "Italians",
                 "Ivorians", "Jamaicans", "Jan Mayen residents", "Japanese", "Channel Islanders",
                 "Jordanians", "Kazakhstanis, Kazakhs", "Kenyans", "I-Kiribati",
                 "Koreans", "Koreans", "Kosovars", "Kuwaitis", "Kyrgyzstanis, Kyrgyz, Kirgiz, Kirghiz",
                 "Laos, Laotians", "Latvians, Letts", "Lebanese", "Basotho (singular Mosotho)",
                 "Liberians", "Libyans", "Liechtensteiners", "Lithuanians", "Luxembourgers",
                 "Macanese, Chinese", "Malagasy", "Malawians", "Malaysians", "Maldivians",
                 "Malians", "Maltese", "Marshallese", "Martiniquais/Martiniquaises",
                 "Mauritanians", "Mauritians", "Mahorans", "Mexicans", "Micronesians",
                 "Moldovans", "Monégasques, Monacans", "Mongolians, Mongols",
                 "Montenegrins", "Montserratians", "Moroccans", "Mozambicans",
                 "Myanmars", "Namibians", "Nauruans", "Nepali, Nepalese", "Dutch, Dutchmen/Dutchwomen, Netherlanders",
                 "New Caledonians", "New Zealanders", "Nicaraguans", "Nigeriens",
                 "Nigerians", "Niueans", "Norfolk Islanders", "Macedonians", "Northern Irish, Northern Irishmen/Northern Irishwomen",
                 "Northern Marianans", "Norwegians", "Omanis", "Pakistanis", "Palauans",
                 "Palestinians", "Panamanians", "Papua New Guineans, Papuans",
                 "Paraguayans", "Peruvians", "Filipinos/Filipinas", "Pitcairn Islanders",
                 "Poles", "Portuguese", "Puerto Ricans", "Qataris", "Réunionese, Réunionnais/Réunionnaises",
                 "Romanians", "Russians", "Rwandans, Banyarwanda", "Saba Dutch",
                 "Barthélemois/Barthélemoises", "Saint Helenians", "Kittitians or Nevisians",
                 "Saint Lucians", "Saint-Martinois/Saint-Martinoises", "Saint-Pierrais/Saint-Pierraises or Miquelonnais/Miquelonnaises",
                 "Saint Vincentians or Vincentians", "Sahrawis, Western Saharans",
                 "Samoans", "Sammarinese", "São Toméans", "Saudis, Saudi Arabians",
                 "Scots, Scotsmen, Scotswomen", "Senegalese", "Serbs, Serbians",
                 "Seychellois/Seychelloises", "Sierra Leoneans", "Singaporeans[3][4]",
                 "Statians", "Sint Maartener", "Slovaks, Slovakians", "Slovenes, Slovenians",
                 "Solomon Islanders", "Somalis", "Somalilanders", "South Africans",
                 "South Georgia Islanders or South Sandwich Islanders", "South Ossetians",
                 "South Sudanese", "Spaniards", "Sri Lankans", "Sudanese", "Surinamers",
                 "Svalbard residents", "Swedes", "Swiss", "Syrians", "Tajikistanis, Tajiks",
                 "Tanzanians", "Thai", "Timorese", "Togolese", "Tokelauans", "Tongans",
                 "Trinidadians or Tobagonians", "Tunisians", "Turks", "Turkmens",
                 "Turks and Caicos Islanders", "Tuvaluans", "Ugandans", "Ukrainians",
                 "Emiratis, Emirians, Emiri", "Britons, British", "Americans[p]",
                 "Uruguayans", "Uzbekistanis, Uzbeks", "Ni-Vanuatu", "Vatican citizens",
                 "Venezuelans", "Vietnamese people", "British Virgin Islanders",
                 "U.S. Virgin Islanders", "Welshmen/Welshwomen, Walian", "Wallis and Futuna Islanders, Wallisians or Futunans",
                 "Sahrawis, Sahraouis", "Yemenis", "Zambians", "Zimbabweans")), class = "data.frame", row.names = c(NA,
                                                                                                                    -263L))
}

