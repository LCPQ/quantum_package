type t =
|H                                                 |He
|Li|Be                              |B |C |N |O |F |Ne
|Na|Mg                              |Al|Si|P |S |Cl|Ar
|K |Ca|Sc|Ti|V |Cr|Mn|Fe|Co|Ni|Cu|Zn|Ga|Ge|As|Se|Br|Kr
;;

let of_string = function
| "H"  | "Hydrogen" -> H
| "He" | "Helium" -> He
| "Li" | "Lithium" -> Li
| "Be" | "Beryllium" -> Be
| "B"  | "Boron" -> B
| "C"  | "Carbon" -> C
| "N"  | "Nitrogen" -> N
| "O"  | "Oxygen" -> O
| "F"  | "Fluorine" -> F
| "Ne" | "Neon" -> Ne
| "Na" | "Sodium" -> Na
| "Mg" | "Magnesium" -> Mg
| "Al" | "Aluminum" -> Al
| "Si" | "Silicon" -> Si
| "P"  | "Phosphorus" -> P
| "S"  | "Sulfur" -> S
| "Cl" | "Chlorine" -> Cl
| "Ar" | "Argon" -> Ar
| "K"  | "Potassium" -> K
| "Ca" | "Calcium" -> Ca
| "Sc" | "Scandium" -> Sc
| "Ti" | "Titanium" -> Ti
| "V"  | "Vanadium" -> V
| "Cr" | "Chromium" -> Cr
| "Mn" | "Manganese" -> Mn
| "Fe" | "Iron" -> Fe
| "Co" | "Cobalt" -> Co
| "Ni" | "Nickel" -> Ni
| "Cu" | "Copper" -> Cu
| "Zn" | "Zinc" -> Zn
| "Ga" | "Gallium" -> Ga
| "Ge" | "Germanium" -> Ge
| "As" | "Arsenic" -> As
| "Se" | "Selenium" -> Se
| "Br" | "Bromine" -> Br
| "Kr" | "Krypton" -> Kr
;;

let to_string = function
| H   -> "H"
| He  -> "He"
| Li  -> "Li"
| Be  -> "Be"
| B   -> "B"
| C   -> "C"
| N   -> "N"
| O   -> "O"
| F   -> "F"
| Ne  -> "Ne"
| Na  -> "Na"
| Mg  -> "Mg"
| Al  -> "Al"
| Si  -> "Si"
| P   -> "P"
| S   -> "S"
| Cl  -> "Cl"
| Ar  -> "Ar"
| K   -> "K"
| Ca  -> "Ca"
| Sc  -> "Sc"
| Ti  -> "Ti"
| V   -> "V"
| Cr  -> "Cr"
| Mn  -> "Mn"
| Fe  -> "Fe"
| Co  -> "Co"
| Ni  -> "Ni"
| Cu  -> "Cu"
| Zn  -> "Zn"
| Ga  -> "Ga"
| Ge  -> "Ge"
| As  -> "As"
| Se  -> "Se"
| Br  -> "Br"
| Kr  -> "Kr"
;;

let to_long_string = function
| H   -> "Hydrogen"
| He  -> "Helium"
| Li  -> "Lithium"
| Be  -> "Beryllium"
| B   -> "Boron"
| C   -> "Carbon"
| N   -> "Nitrogen"
| O   -> "Oxygen"
| F   -> "Fluorine"
| Ne  -> "Neon"
| Na  -> "Sodium"
| Mg  -> "Magnesium"
| Al  -> "Aluminum"
| Si  -> "Silicon"
| P   -> "Phosphorus"
| S   -> "Sulfur"
| Cl  -> "Chlorine"
| Ar  -> "Argon"
| K   -> "Potassium"
| Ca  -> "Calcium"
| Sc  -> "Scandium"
| Ti  -> "Titanium"
| V   -> "Vanadium"
| Cr  -> "Chromium"
| Mn  -> "Manganese"
| Fe  -> "Iron"
| Co  -> "Cobalt"
| Ni  -> "Nickel"
| Cu  -> "Copper"
| Zn  -> "Zinc"
| Ga  -> "Gallium"
| Ge  -> "Germanium"
| As  -> "Arsenic"
| Se  -> "Selenium"
| Br  -> "Bromine"
| Kr  -> "Krypton"
;;
