module Data.DateTime.Gregorian.TimeZones where
import Control.Arrow
import Data.Normalize
import Text.Format.Write
import Text.Printf (printf)

data TimeZone = TZ
    { code   :: String
    , full   :: String
    , use    :: String
    , hour   :: Int
    , minute :: Int
    }

instance Show TimeZone where show = showTz Numeric

data ShowStyle
    = Numeric
    | HourMinute
    | HourMinuteSecond
    | Minimal deriving (Eq, Ord, Enum, Read, Show)

utcOffset :: TimeZone -> Int
utcOffset tz = (hour tz * 60 * 60) + (minute tz * 60)

universal :: TimeZone
universal = TZ "UTC" "Coordinated Universal Time" "" 0 0

showTz :: ShowStyle -> TimeZone -> String
showTz sty tz = let
    t = hour tz * 100 + minute tz
    sign = case compare t 0 of
        EQ -> '±'
        LT -> '-'
        GT -> '+'
    out = case sty of
        Numeric -> printf "%c%d%d"
        HourMinute -> printf "%c%d:%d"
        HourMinuteSecond -> printf "%c%d:%d:00"
        Minimal -> \c h m ->
            if m == 0 then printf "%c%d" c h else printf "%c%d:%d" c h m
    in out sign (hour tz) (minute tz)

instance Normalize TimeZone where
    isNormal tz = hour tz == 0 && minute tz == 0
    normalize = overflow &&& const universal
    overflow = utcOffset

instance Formattable TimeZone where
    number tz = toInteger $ hour tz * 100 + minute tz
    representation = showTz . toEnum
    width = const 4
    name = code

timeZones :: [TimeZone]
timeZones =
    [ TZ "A"     "Alpha Time Zone"                    "Military"        (1)  00
    , TZ "ADT"   "Atlantic Daylight Time"             "Atlantic"        (-3)  00
    , TZ "ADT"   "Atlantic Daylight Time"             "North America"   (-3)  00
    , TZ "AFT"   "Afghanistan Time"                   "Asia"            (4)  30
    , TZ "AKDT"  "Alaska Daylight Time"               "North America"   (-8)  00
    , TZ "AKST"  "Alaska Standard Time"               "North America"   (-9)  00
    , TZ "ALMT"  "Alma-Ata Time"                      "Asia"            (6)  00
    , TZ "AMST"  "Armenia Summer Time"                "Asia"            (5)  00
    , TZ "AMST"  "Amazon Summer Time"                 "South America"   (-3)  00
    , TZ "AMT"   "Armenia Time"                       "Asia"            (4)  00
    , TZ "AMT"   "Amazon Time"                        "South America"   (-4)  00
    , TZ "ANAST" "Anadyr Summer Time"                 "Asia"            (12) 00
    , TZ "ANAT"  "Anadyr Time"                        "Asia"            (12) 00
    , TZ "AQTT"  "Aqtobe Time"                        "Asia"            (5)  00
    , TZ "ART"   "Argentina Time"                     "South America"   (-3)  00
    , TZ "AST"   "Arabia Standard Time"               "Asia"            (3)  00
    , TZ "AST"   "Atlantic Standard Time"             "Atlantic"        (-4)  00
    , TZ "AST"   "Atlantic Standard Time"             "Caribbean"       (-4)  00
    , TZ "AST"   "Atlantic Standard Time"             "North America"   (-4)  00
    , TZ "AZOST" "Azores Summer Time"                 "Atlantic"        0     00
    , TZ "AZOT"  "Azores Time"                        "Atlantic"        (-1)  00
    , TZ "AZST"  "Azerbaijan Summer Time"             "Asia"            (5)  00
    , TZ "AZT"   "Azerbaijan Time"                    "Asia"            (4)  00
    , TZ "B"     "Bravo Time Zone"                    "Military"        (2)  00
    , TZ "BNT"   "Brunei Darussalam Time"             "Asia"            (8)  00
    , TZ "BOT"   "Bolivia Time"                       "South America"   (-4)  00
    , TZ "BRST"  "Brasilia Summer Time"               "South America"   (-2)  00
    , TZ "BRT"   "Brasília time"                      "South America"   (-3)  00
    , TZ "BST"   "Bangladesh Standard Time"           "Asia"            (6)  00
    , TZ "BST"   "British Summer Time"                "Europe"          (1)  00
    , TZ "BTT"   "Bhutan Time"                        "Asia"            (6)  00
    , TZ "C"     "Charlie Time Zone"                  "Military"        (3)  00
    , TZ "CAST"  "Casey Time"                         "Antarctica"      (8)  00
    , TZ "CAT"   "Central Africa Time"                "Africa"          (2)  00
    , TZ "CCT"   "Cocos Islands Time"                 "Indian Ocean"    (6)  30
    , TZ "CDT"   "Central Daylight Time"              "Australia"       (10) 30
    , TZ "CDT"   "Cuba Daylight Time"                 "Caribbean"       (-4)  00
    , TZ "CDT"   "Central Daylight Time"              "North America"   (-5)  00
    , TZ "CEST"  "Central European Summer Time"       "Europe"          (2)  00
    , TZ "CET"   "Central European Time"              "Africa"          (1)  00
    , TZ "CET"   "Central European Time"              "Europe"          (1)  00
    , TZ "CHADT" "Chatham Island Daylight Time"       "Pacific"         (13) 45
    , TZ "CHAST" "Chatham Island Standard Time"       "Pacific"         (12) 45
    , TZ "CKT"   "Cook Island Time"                   "Pacific"         (-10) 00
    , TZ "CLST"  "Chile Summer Time"                  "South America"   (-3)  00
    , TZ "CLT"   "Chile Standard Time"                "South America"   (-4)  00
    , TZ "COT"   "Colombia Time"                      "South America"   (-5)  00
    , TZ "CST"   "China Standard Time"                "Asia"            (8)  00
    , TZ "CST"   "Central Standard Time"              "Australia"       (9)  30
    , TZ "CST"   "Central Standard Time"              "Central America" (-6)  00
    , TZ "CST"   "Cuba Standard Time"                 "Caribbean"       (-5)  00
    , TZ "CST"   "Central Standard Time"              "North America"   (-6)  00
    , TZ "CVT"   "Cape Verde Time"                    "Africa"          (-1)  00
    , TZ "CXT"   "Christmas Island Time"              "Australia"       (7)  00
    , TZ "ChST"  "Chamorro Standard Time"             "Pacific"         (10) 00
    , TZ "D"     "Delta Time Zone"                    "Military"        (4)  00
    , TZ "DAVT"  "Davis Time"                         "Antarctica"      (7)  00
    , TZ "E"     "Echo Time Zone"                     "Military"        (5)  00
    , TZ "EASST" "Easter Island Summer Time"          "Pacific"         (-5)  00
    , TZ "EAST"  "Easter Island Standard Time"        "Pacific"         (-6)  00
    , TZ "EAT"   "Eastern Africa Time"                "Africa"          (3)  00
    , TZ "EAT"   "East Africa Time"                   "Indian Ocean"    (3)  00
    , TZ "ECT"   "Ecuador Time"                       "South America"   (-5)  00
    , TZ "EDT"   "Eastern Daylight Time"              "Australia"       (11) 00
    , TZ "EDT"   "Eastern Daylight Time"              "Caribbean"       (-4)  00
    , TZ "EDT"   "Eastern Daylight Time"              "North America"   (-4)  00
    , TZ "EDT"   "Eastern Daylight Time"              "Pacific"         (11) 00
    , TZ "EEST"  "Eastern European Summer Time"       "Africa"          (3)  00
    , TZ "EEST"  "Eastern European Summer Time"       "Asia"            (3)  00
    , TZ "EEST"  "Eastern European Summer Time"       "Europe"          (3)  00
    , TZ "EET"   "Eastern European Time"              "Africa"          (2)  00
    , TZ "EET"   "Eastern European Time"              "Asia"            (2)  00
    , TZ "EET"   "Eastern European Time"              "Europe"          (2)  00
    , TZ "EGST"  "Eastern Greenland Summer Time"      "North America"   0     00
    , TZ "EGT"   "East Greenland Time"                "North America"   (-1)  00
    , TZ "EST"   "Eastern Standard Time"              "Australia"       (10) 00
    , TZ "EST"   "Eastern Standard Time"              "Central America" (-5)  00
    , TZ "EST"   "Eastern Standard Time"              "Caribbean"       (-5)  00
    , TZ "EST"   "Eastern Standard Time"              "North America"   (-5)  00
    , TZ "ET"    "Tiempo del Este"                    "Central America" (-5)  00
    , TZ "ET"    "Tiempo del Este"                    "Caribbean"       (-5)  00
    , TZ "ET"    "Tiempo Del Este"                    "North America"   (-5)  00
    , TZ "F"     "Foxtrot Time Zone"                  "Military"        (6)  00
    , TZ "FJST"  "Fiji Summer Time"                   "Pacific"         (13) 00
    , TZ "FJT"   "Fiji Time"                          "Pacific"         (12) 00
    , TZ "FKST"  "Falkland Islands Summer Time"       "South America"   (-3)  00
    , TZ "FKT"   "Falkland Island Time"               "South America"   (-4)  00
    , TZ "FNT"   "Fernando de Noronha Time"           "South America"   (-2)  00
    , TZ "G"     "Golf Time Zone"                     "Military"        (7)  00
    , TZ "GALT"  "Galapagos Time"                     "Pacific"         (-6)  00
    , TZ "GAMT"  "Gambier Time"                       "Pacific"         (-9)  00
    , TZ "GET"   "Georgia Standard Time"              "Asia"            (4)  00
    , TZ "GFT"   "French Guiana Time"                 "South America"   (-3)  00
    , TZ "GILT"  "Gilbert Island Time"                "Pacific"         (12) 00
    , TZ "GMT"   "Greenwich Mean Time"                "Africa"          0     00
    , TZ "GMT"   "Greenwich Mean Time"                "Europe"          0     00
    , TZ "GST"   "Gulf Standard Time"                 "Asia"            (4)  00
    , TZ "GYT"   "Guyana Time"                        "South America"   (-4)  00
    , TZ "H"     "Hotel Time Zone"                    "Military"        (8)  00
    , TZ "HAA"   "Heure Avancée de l'Atlantique"      "Atlantic"        (-3)  00
    , TZ "HAA"   "Heure Avancée de l'Atlantique"      "North America"   (-3)  00
    , TZ "HAC"   "Heure Avancée du Centre"            "North America"   (-5)  00
    , TZ "HADT"  "Hawaii-Aleutian Daylight Time"      "North America"   (-9)  00
    , TZ "HAE"   "Heure Avancée de l'Est"             "Caribbean"       (-4)  00
    , TZ "HAE"   "Heure Avancée de l'Est"             "North America"   (-4)  00
    , TZ "HAP"   "Heure Avancée du Pacifique"         "North America"   (-7)  00
    , TZ "HAR"   "Heure Avancée des Rocheuses"        "North America"   (-6)  00
    , TZ "HAST"  "Hawaii-Aleutian Standard Time"      "North America"   (-10) 00
    , TZ "HAT"   "Heure Avancée de Terre-Neuve"       "North America"   (-2)  30
    , TZ "HAY"   "Heure Avancée du Yukon"             "North America"   (-8)  00
    , TZ "HKT"   "Hong Kong Time"                     "Asia"            (8)  00
    , TZ "HLV"   "Hora Legal de Venezuela"            "South America"   (-4)  30
    , TZ "HNA"   "Heure Normale de l'Atlantique"      "Atlantic"        (-4)  00
    , TZ "HNA"   "Heure Normale de l'Atlantique"      "Caribbean"       (-4)  00
    , TZ "HNA"   "Heure Normale de l'Atlantique"      "North America"   (-4)  00
    , TZ "HNC"   "Heure Normale du Centre"            "Central America" (-6)  00
    , TZ "HNC"   "Heure Normale du Centre"            "North America"   (-6)  00
    , TZ "HNE"   "Heure Normale de l'Est"             "Central America" (-5)  00
    , TZ "HNE"   "Heure Normale de l'Est"             "Caribbean"       (-5)  00
    , TZ "HNE"   "Heure Normale de l'Est"             "North America"   (-5)  00
    , TZ "HNP"   "Heure Normale du Pacifique"         "North America"   (-8)  00
    , TZ "HNR"   "Heure Normale des Rocheuses"        "North America"   (-7)  00
    , TZ "HNT"   "Heure Normale de Terre-Neuve"       "North America"   (-3)  30
    , TZ "HNY"   "Heure Normale du Yukon"             "North America"   (-9)  00
    , TZ "HOVT"  "Hovd Time"                          "Asia"            (7)  00
    , TZ "I"     "India Time Zone"                    "Military"        (9)  00
    , TZ "ICT"   "Indochina Time"                     "Asia"            (7)  00
    , TZ "IDT"   "Israel Daylight Time"               "Asia"            (3)  00
    , TZ "IOT"   "Indian Chagos Time"                 "Indian Ocean"    (6)  00
    , TZ "IRDT"  "Iran Daylight Time"                 "Asia"            (4)  30
    , TZ "IRKST" "Irkutsk Summer Time"                "Asia"            (9)  00
    , TZ "IRKT"  "Irkutsk Time"                       "Asia"            (9)  00
    , TZ "IRST"  "Iran Standard Time"                 "Asia"            (3)  30
    , TZ "IST"   "Israel Standard Time"               "Asia"            (2)  00
    , TZ "IST"   "India Standard Time"                "Asia"            (5)  30
    , TZ "IST"   "Irish Standard Time"                "Europe"          (1)  00
    , TZ "JST"   "Japan Standard Time"                "Asia"            (9)  00
    , TZ "K"     "Kilo Time Zone"                     "Military"        (10) 00
    , TZ "KGT"   "Kyrgyzstan Time"                    "Asia"            (6)  00
    , TZ "KRAST" "Krasnoyarsk Summer Time"            "Asia"            (8)  00
    , TZ "KRAT"  "Krasnoyarsk Time"                   "Asia"            (8)  00
    , TZ "KST"   "Korea Standard Time"                "Asia"            (9)  00
    , TZ "KUYT"  "Kuybyshev Time"                     "Europe"          (4)  00
    , TZ "L"     "Lima Time Zone"                     "Military"        (11) 00
    , TZ "LHDT"  "Lord Howe Daylight Time"            "Australia"       (11) 00
    , TZ "LHST"  "Lord Howe Standard Time"            "Australia"       (10) 30
    , TZ "LINT"  "Line Islands Time"                  "Pacific"         (14) 00
    , TZ "M"     "Mike Time Zone"                     "Military"        (12) 00
    , TZ "MAGST" "Magadan Summer Time"                "Asia"            (12) 00
    , TZ "MAGT"  "Magadan Time"                       "Asia"            (12) 00
    , TZ "MART"  "Marquesas Time"                     "Pacific"         (-9)  30
    , TZ "MAWT"  "Mawson Time"                        "Antarctica"      (5)  00
    , TZ "MDT"   "Mountain Daylight Time"             "North America"   (-6)  00
    , TZ "MESZ"  "Mitteleuropäische Sommerzeit"       "Europe"          (2)  00
    , TZ "MEZ"   "Mitteleuropäische Zeit"             "Africa"          (1)  00
    , TZ "MHT"   "Marshall Islands Time"              "Pacific"         (12) 00
    , TZ "MMT"   "Myanmar Time"                       "Asia"            (6)  30
    , TZ "MSD"   "Moscow Daylight Time"               "Europe"          (4)  00
    , TZ "MSK"   "Moscow Standard Time"               "Europe"          (4)  00
    , TZ "MST"   "Mountain Standard Time"             "North America"   (-7)  00
    , TZ "MUT"   "Mauritius Time"                     "Africa"          (4)  00
    , TZ "MVT"   "Maldives Time"                      "Asia"            (5)  00
    , TZ "MYT"   "Malaysia Time"                      "Asia"            (8)  00
    , TZ "N"     "November Time Zone"                 "Military"        (-1)  00
    , TZ "NCT"   "New Caledonia Time"                 "Pacific"         (11) 00
    , TZ "NDT"   "Newfoundland Daylight Time"         "North America"   (-2)  30
    , TZ "NFT"   "Norfolk Time"                       "Australia"       (11) 30
    , TZ "NOVST" "Novosibirsk Summer Time"            "Asia"            (7)  00
    , TZ "NOVT"  "Novosibirsk Time"                   "Asia"            (6)  00
    , TZ "NPT"   "Nepal Time"                         "Asia"            (5)  45
    , TZ "NST"   "Newfoundland Standard Time"         "North America"   (-3)  30
    , TZ "NUT"   "Niue Time"                          "Pacific"         (-11) 00
    , TZ "NZDT"  "New Zealand Daylight Time"          "Antarctica"      (13) 00
    , TZ "NZDT"  "New Zealand Daylight Time"          "Pacific"         (13) 00
    , TZ "NZST"  "New Zealand Standard Time"          "Antarctica"      (12) 00
    , TZ "NZST"  "New Zealand Standard Time"          "Pacific"         (12) 00
    , TZ "O"     "Oscar Time Zone"                    "Military"        (-2)  00
    , TZ "OMSST" "Omsk Summer Time"                   "Asia"            (7)  00
    , TZ "OMST"  "Omsk Standard Time"                 "Asia"            (7)  00
    , TZ "P"     "Papa Time Zone"                     "Military"        (-3)  00
    , TZ "PDT"   "Pacific Daylight Time"              "North America"   (-7)  00
    , TZ "PET"   "Peru Time"                          "South America"   (-5)  00
    , TZ "PETST" "Kamchatka Summer Time"              "Asia"            (12) 00
    , TZ "PETT"  "Kamchatka Time"                     "Asia"            (12) 00
    , TZ "PGT"   "Papua New Guinea Time"              "Pacific"         (10) 00
    , TZ "PHOT"  "Phoenix Island Time"                "Pacific"         (13) 00
    , TZ "PHT"   "Philippine Time"                    "Asia"            (8)  00
    , TZ "PKT"   "Pakistan Standard Time"             "Asia"            (5)  00
    , TZ "PMDT"  "Pierre & Miquelon Daylight Time"    "North America"   (-2)  00
    , TZ "PMST"  "Pierre & Miquelon Standard Time"    "North America"   (-3)  00
    , TZ "PONT"  "Pohnpei Standard Time"              "Pacific"         (11) 00
    , TZ "PST"   "Pacific Standard Time"              "North America"   (-8)  00
    , TZ "PST"   "Pitcairn Standard Time"             "Pacific"         (-8)  00
    , TZ "PT"    "Tiempo del Pacífico"                "North America"   (-8)  00
    , TZ "PWT"   "Palau Time"                         "Pacific"         (9)  00
    , TZ "PYST"  "Paraguay Summer Time"               "South America"   (-3)  00
    , TZ "PYT"   "Paraguay Time"                      "South America"   (-4)  00
    , TZ "Q"     "Quebec Time Zone"                   "Military"        (-4)  00
    , TZ "R"     "Romeo Time Zone"                    "Military"        (-5)  00
    , TZ "RET"   "Reunion Time"                       "Africa"          (4)  00
    , TZ "S"     "Sierra Time Zone"                   "Military"        (-6)  00
    , TZ "SAMT"  "Samara Time"                        "Europe"          (4)  00
    , TZ "SAST"  "South Africa Standard Time"         "Africa"          (2)  00
    , TZ "SBT"   "Solomon IslandsTime"                "Pacific"         (11) 00
    , TZ "SCT"   "Seychelles Time"                    "Africa"          (4)  00
    , TZ "SGT"   "Singapore Time"                     "Asia"            (8)  00
    , TZ "SRT"   "Suriname Time"                      "South America"   (-3)  00
    , TZ "SST"   "Samoa Standard Time"                "Pacific"         (-11) 00
    , TZ "T"     "Tango Time Zone"                    "Military"        (-7)  00
    , TZ "TAHT"  "Tahiti Time"                        "Pacific"         (-10) 00
    , TZ "TFT"   "French Southern and Antarctic Time" "Indian Ocean"    (5)  00
    , TZ "TJT"   "Tajikistan Time"                    "Asia"            (5)  00
    , TZ "TKT"   "Tokelau Time"                       "Pacific"         (-10) 00
    , TZ "TLT"   "East Timor Time"                    "Asia"            (9)  00
    , TZ "TMT"   "Turkmenistan Time"                  "Asia"            (5)  00
    , TZ "TVT"   "Tuvalu Time"                        "Pacific"         (12) 00
    , TZ "U"     "Uniform Time Zone"                  "Military"        (-8)  00
    , TZ "ULAT"  "Ulaanbaatar Time"                   "Asia"            (8)  00
    , TZ "UYST"  "Uruguay Summer Time"                "South America"   (-2)  00
    , TZ "UYT"   "Uruguay Time"                       "South America"   (-3)  00
    , TZ "UZT"   "Uzbekistan Time"                    "Asia"            (5)  00
    , TZ "V"     "Victor Time Zone"                   "Military"        (-9)  00
    , TZ "VET"   "Venezuelan Standard Time"           "South America"   (-4)  30
    , TZ "VLAST" "Vladivostok Summer Time"            "Asia"            (11) 00
    , TZ "VLAT"  "Vladivostok Time"                   "Asia"            (11) 00
    , TZ "VUT"   "Vanuatu Time"                       "Pacific"         (11) 00
    , TZ "W"     "Whiskey Time Zone"                  "Military"        (-10) 00
    , TZ "WAST"  "West Africa Summer Time"            "Africa"          (2)  00
    , TZ "WAT"   "West Africa Time"                   "Africa"          (1)  00
    , TZ "WDT"   "Western Daylight Time"              "Australia"       (9)  00
    , TZ "WEST"  "Western European Summer Time"       "Africa"          (1)  00
    , TZ "WEST"  "Western European Summer Time"       "Europe"          (1)  00
    , TZ "WESZ"  "Westeuropäische Sommerzeit"         "Africa"          (1)  00
    , TZ "WET"   "Western European Time"              "Africa"          0     00
    , TZ "WET"   "Western European Time"              "Europe"          0     00
    , TZ "WEZ"   "Westeuropäische Zeit"               "Europe"          0     00
    , TZ "WFT"   "Wallis and Futuna Time"             "Pacific"         (12) 00
    , TZ "WGST"  "Western Greenland Summer Time"      "North America"   (-2)  00
    , TZ "WGT"   "West Greenland Time"                "North America"   (-3)  00
    , TZ "WIB"   "Western Indonesian Time"            "Asia"            (7)  00
    , TZ "WIT"   "Eastern Indonesian Time"            "Asia"            (9)  00
    , TZ "WITA"  "Central Indonesian Time"            "Asia"            (8)  00
    , TZ "WST"   "Western Sahara Summer Time"         "Africa"          (1)  00
    , TZ "WST"   "Western Standard Time"              "Australia"       (8)  00
    , TZ "WST"   "West Samoa Time"                    "Pacific"         (13) 00
    , TZ "WT"    "Western Sahara Standard Time"       "Africa"          0     00
    , TZ "X"     "X-ray Time Zone"                    "Military"        (-11) 00
    , TZ "Y"     "Yankee Time Zone"                   "Military"        (-12) 00
    , TZ "YAKST" "Yakutsk Summer Time"                "Asia"            (10) 00
    , TZ "YAKT"  "Yakutsk Time"                       "Asia"            (10) 00
    , TZ "YAPT"  "Yap Time"                           "Pacific"         (10) 00
    , TZ "YEKST" "Yekaterinburg Summer Time"          "Asia"            (6)  00
    , TZ "YEKT"  " Yekaterinburg Time"                "Asia"            (6)  00
    , TZ "Z"     "Zulu Time Zone"                     "Military"        0     00
    ]
