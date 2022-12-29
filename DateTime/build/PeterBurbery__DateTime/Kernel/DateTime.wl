(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["PeterBurbery`DateTime`"];


(* ::Text:: *)
(*Declare your public symbols here:*)


NextLeapYear;
PreviousLeapYear;
DatePlusLeapYear;
LeapYearRange;
LeapYearDay;
NextLeapYearDay;
PreviousLeapYearDay;
DatePlusLeapYearDay;
LeapYearDayRange;
ISOWeekDate;
FromISOWeekDate;
FromISOTimestamp;
EasterSunday;
EasterSundayGreekOrthodox;
(*LifetimeChart;
ClockHandsAngles;
RelativeTimeString;*)


Begin["`Private`"];


(* ::Section:: *)
(*Definitions*)


(* ::Text:: *)
(*Define your public and private symbols here:*)


NextLeapYear // ClearAll

NextLeapYear[date_?DateObjectQ] :=
    NestWhile[NextDate[#, "Year"]&, date, !LeapYearQ[#]&]

NextLeapYear[] :=
    NextLeapYear[CurrentDate["Year"]]

PreviousLeapYear // ClearAll

PreviousLeapYear[date_?DateObjectQ] :=
    NestWhile[PreviousDate[#, "Year"]&, date, !LeapYearQ[#]&]

PreviousLeapYear[] :=
    PreviousLeapYear[CurrentDate["Year"]]

DatePlusLeapYear // ClearAll

DatePlusLeapYear[date_?DateObjectQ, steps_] :=
    Block[{x, count},
        x = date;
        count = 0;
        Which[
            steps \[Element] NegativeIntegers,
                Until[
                    count == Abs[steps]
                    ,
                    x //= PreviousDate[#, "Year"]&;
                    count //=
                        Which[
                            LeapYearQ[x],
                                # + 1
                            ,
                            !LeapYearQ[x],
                                #
                        ]&;
                ];
                x
            ,
            steps \[Element] PositiveIntegers,
                Until[
                    count == steps
                    ,
                    x //= NextDate[#, "Year"]&;
                    count //=
                        Which[
                            LeapYearQ[x],
                                # + 1
                            ,
                            !LeapYearQ[x],
                                #
                        ]&;
                ];
                x
        ]
    ]

DatePlusLeapYear[steps_] :=
    DatePlusLeapYear[CurrentDate["Year"], steps]

LeapYearRange // ClearAll

LeapYearRange[date_?DateObjectQ, steps_] :=
    Block[{x, count, list},
        x = date;
        count = 0;
        list = {};
        Which[
            steps \[Element] NegativeIntegers,
                Until[
                    count == Abs[steps]
                    ,
                    x //= PreviousDate[#, "Year"]&;
                    count //=
                        Which[
                            LeapYearQ[x],
                                list = Join[list, {x}];
                                # + 1
                            ,
                            !LeapYearQ[x],
                                #
                        ]&;
                ];
                Sort @ list
            ,
            steps \[Element] PositiveIntegers,
                Until[
                    count == steps
                    ,
                    x //= NextDate[#, "Year"]&;
                    count //=
                        Which[
                            LeapYearQ[x],
                                list = Join[list, {x}];
                                # + 1
                            ,
                            !LeapYearQ[x],
                                #
                        ]&;
                ];
                Sort @ list
        ]
    ]

LeapYearRange[steps_] :=
    LeapYearRange[CurrentDate["Year"], steps]

LeapYearDay // ClearAll

Attributes[LeapYearDay] = {Listable};

LeapYearDay[date_?DateObjectQ] /; LeapYearQ[date] :=
    FromDateString[StringJoin["29 February ", DateString[date, "Year"
        ]]]

NextLeapYearDay // ClearAll

NextLeapYearDay[date_?DateObjectQ] :=
    LeapYearDay[NextLeapYear[date]]

NextLeapYearDay[] :=
    LeapYearDay[NextLeapYear[]]

PreviousLeapYearDay // ClearAll

PreviousLeapYearDay[date_?DateObjectQ] :=
    LeapYearDay[PreviousLeapYear[date]]

PreviousLeapYearDay[] :=
    LeapYearDay[PreviousLeapYear[]]

DatePlusLeapYearDay // ClearAll

DatePlusLeapYearDay[date_?DateObjectQ, steps_] :=
    LeapYearDay[DatePlusLeapYear[date, steps]]

DatePlusLeapYearDay[steps_] :=
    DatePlusLeapYearDay[CurrentDate["Year"], steps]

LeapYearDayRange // ClearAll

LeapYearDayRange[date_?DateObjectQ, steps_] :=
    LeapYearDay[LeapYearRange[date, steps]]

LeapYearDayRange[steps_] :=
    LeapYearDayRange[CurrentDate["Year"], steps]

ISOWeekDate // ClearAll

ISOWeekDate[date_?DateObjectQ] :=
    StringTemplate["<* DateValue[#1,\"Year\"] *>-W<* DateValue[#1,\"ISOWeek\"] *>-<* DateValue[#1,\"ISOWeekDay\"] *>"
        ][date]

ISOWeekDate[] :=
    ISOWeekDate[Now]

FromISOWeekDate // ClearAll

FromISOWeekDate[{year_?IntegerQ, week_?IntegerQ, day_?IntegerQ}] /; IntervalMemberQ[
    Interval[{1, 52}], week] :=
    DatePlus[DateObject[ToString[year] <> "-W" <> ToString[week] <> "-"
         <> ToString[day]], {1, "Weeks"}] /; IntervalMemberQ[Interval[{1, 7}],
         day]

zeroOrOne[patt_] :=
    Repeated[patt, {0, 1}]

(* matches a timezone suffix like "+08:00" or "Z" *)

timeZonePattern =
    (
        timeZoneString :
            zeroOrOne[
                Alternatives[
                    StringExpression[
                        (* Sign *)zeroOrOne["+" | "-"]
                        ,
                        (* Hours offset *)
                        timeZoneHour : Repeated[DigitCharacter, {2}]
                        ,
                        zeroOrOne[
                            StringExpression[
                                ":" | ""
                                ,
                                (* Minute offset *)
                                timeZoneMinute : Repeated[DigitCharacter,
                                     {2}]
                            ]
                        ]
                    ]
                    ,
                    (* Zulu (UTC) *)
                    "Z"
                ]
            ]
    );

durationNumberPattern = Alternatives[DigitCharacter, ".", ",", "+", "-"
    ]..;

isoDurationPattern = StringExpression[StartOfString, "P", zeroOrOne[years
    :durationNumberPattern ~~ "Y"], zeroOrOne[months:durationNumberPattern
     ~~ "M"], zeroOrOne[weeks:durationNumberPattern ~~ "W"], zeroOrOne[days
    :durationNumberPattern ~~ "D"], zeroOrOne[StringExpression["T", zeroOrOne[
    hours:durationNumberPattern ~~ "H"], zeroOrOne[minutes:durationNumberPattern
     ~~ "M"], zeroOrOne[seconds:durationNumberPattern ~~ "S"]]], EndOfString
    ];

(* matches an ISO 8601 date/time string *)

isoTimestampPattern =
    Longest @
        StringExpression[
            StartOfString
            ,
            (* Date *)
            zeroOrOne @
                StringExpression[
                    (* Year *)year :
                        Alternatives[
                            (* Standard 4-digit year *)Repeated[DigitCharacter,
                                 {4, 4}]
                            ,
                                   (* Extended year, 4-6 digits prefixed
                                
                                
                                 with
                                 mandatory sign *)
                            StringExpression["+" | "-", Repeated[DigitCharacter,
                                 {4, 6}]]
                        ]
                    ,
                    zeroOrOne[
                        StringExpression[
                            zeroOrOne["-"]
                            ,
                            Alternatives[
                                (* Month and day *)StringExpression[month
                                     : Repeated[DigitCharacter, {2}], zeroOrOne[StringExpression[zeroOrOne[
                                    "-"], day : Repeated[DigitCharacter, {2}]]]]
                                ,
                                (* Week number and weekday number *)
                                StringExpression["W", weekNumber : Repeated[
                                    DigitCharacter, {2}], zeroOrOne["-"], weekDay : Repeated[DigitCharacter,
                                     {0, 1}]]
                                ,
                                     (* Ordinal day (days since start
                                    
                                    
                                     of year
                                    ) *)
                                StringExpression[ordinalDay : Repeated[
                                    DigitCharacter, {3}]]
                            ]
                        ]
                    ]
                ]
            ,
            (* Time *)
            zeroOrOne @
                StringExpression[
                    zeroOrOne["T"]
                    ,
                    StringExpression[
                        (* Hour *)hour : Repeated[DigitCharacter, {2}
                            ]
                        ,
                        zeroOrOne[
                            StringExpression[
                                ":" | ""
                                ,
                                (* Minute *)
                                minute : Repeated[DigitCharacter, {2}
                                    ]
                                ,
                                zeroOrOne[
                                    StringExpression[
                                        ":" | ""
                                        ,
                                        (* Second *)
                                        second : Repeated[DigitCharacter,
                                             {2}]
                                        ,
                                        (* Second fraction *)
                                        secondFraction : zeroOrOne[StringExpression[
                                            ".", Repeated[DigitCharacter]]]
                                    ]
                                ]
                            ]
                        ]
                        ,
                        (* Time zone suffix *)
                        timeZoneString : (timeZonePattern //. Verbatim[
                            Pattern][name_, patt_] :> patt)
                    ]
                ]
            ,
            EndOfString
        ];

patternRule[pattern_] :=
    pattern :> Evaluate @ Cases[pattern, Verbatim[Pattern][name_, ___]
         :> (Capitalize[StringSplit[ToString @ name, "`"][[-1]], "TitleCase"] 
        -> name), All]

isoIntermediateForm[str_] :=
    DeleteCases[SelectFirst[Association /@ StringCases[str, patternRule
         @ isoTimestampPattern], Values /* StringJoin /* StringLength /* GreaterThan[
        0], $Failed], ""]


parseTimeZone[str_,defaultTZ_]:=Module[{
preparse=Switch[
str,
"",defaultTZ,
"Z",0,
_,Association/@StringCases[str,patternRule@Longest@timeZonePattern,1]
],
sign,
offsetAbs
},
If[NumberQ@preparse,Return@preparse];
If[Length@preparse<=0,Return@$Failed];

preparse=preparse[[1]];
If[
!StringQ@preparse["TimeZoneHour"]||StringLength[preparse["TimeZoneHour"]]=!=2,
Return@$Failed
];

sign=If[StringTake[preparse["TimeZoneString"],1]==="-",-1,+1];
offsetAbs=ToExpression[preparse["TimeZoneHour"]];

If[
KeyExistsQ[preparse,"TimeZoneMinute"]&&StringLength[preparse["TimeZoneMinute"]]===2,
offsetAbs+=ToExpression[preparse["TimeZoneMinute"]]/60
];

Return@N[offsetAbs*sign]
]        


parseISODuration[str_String] :=
    Module[{
        parseFailure = Failure["ParseFailure", <|"MessageTemplate" :> 
            FromISOTimestamp::invdur, "MessageParameters" -> {str}, "Input" -> str|>
            ]
        ,
        matches =
            Association /@
                StringCases[
                    StringCases[
                        str //
                            StringReplace[
                                {
                                    FromCharacterCode[8722] -> "-"
                                    ,
                                        (* U+2212 minus sign to hyphen
                                        
                                        
                                        
                                         
                                        *)
                                    "," -> "."(* comma decimal separator
                                        
                                        
                                        
                                        
                                         to period *)
                                }
                            ]
                        ,
                        patternRule[isoDurationPattern]
                    ]
                ]
        ,
        match
    },
        If[Length @ matches === 0,
            Return @ parseFailure
        ];
        match = ToExpression /@ DeleteCases[matches[[1]], ""];
        If[Length @ match === 0,
            Return @ parseFailure
        ];
        Quantity[MixedMagnitude[Values @ match], MixedUnit[Keys @ match
            ]] /.
            {
     (* if there's only a single unit, simplify 
                    the quantity
                     to remove the unnecessary MixedMagnitude/MixedUnit
    
    
    
     *)Quantity[MixedMagnitude[{magnitude_}], MixedUnit[{unit_}]] :> Quantity[
    magnitude, unit]
            }
    ]

Options[FromISOTimestamp] = {CalendarType -> Automatic, DateFormat -> Automatic,
     TimeZone :> $TimeZone}

FromISOTimestamp::invstr = "The string \"`1`\" is not a valid ISO 8601 date/time string.";

FromISOTimestamp::invdur = "The string \"`1`\" is not a valid ISO 8601 duration string.";

FromISOTimestamp::invtz = "The timezone specification \"`1`\" is invalid; using value of TimeZone option (`2`) instead.";

FromISOTimestamp[str_String, granularity : (_ ? (Not @* OptionQ)) : Automatic,
     OptionsPattern[]] :=
    Module[{intermediate, yearJan4, numberOfDaysInYear = (DayCount[DateObject[
        {#, 1, 1}], DateObject[{#, 12, 32}]]&), lookupOrder, dateSpec, dateObject,
         dateObjectGranularity = granularity},
        If[(* if the string starts with "P" *)StringStartsQ[str, "P"],
            
            (* then it's a duration; parse it with parseISODuration *)
                
            Return @ parseISODuration @ str
        ];
             (* parse the string to an intermediate form with isoIntermediateForm
            
             *)
        intermediate =
            isoIntermediateForm[
                str //
                    StringReplace[
                        {
                            FromCharacterCode[8722] -> "-"
                            , (* U+2212 minus sign to hyphen *)
                            "," -> "."(* comma decimal separator to period
                                
                                 *)
                        }
                    ]
            ];
        If[(* if the string parsing stage failed *)FailureQ @ intermediate,
            
            (* return a Failure *)
            Return @ Failure["ParseFailure", <|"MessageTemplate" :> FromISOTimestamp
                ::invstr, "MessageParameters" -> {str}, "Input" -> str|>]
        ];
             (* convert all component strings except TimeZoneString to
             numbers
             *)
                   (* ToExpression is safe to use here because we've 
            very carefully
             filtered the contents of the strings *)
        intermediate = intermediate // MapAt[ToExpression, Key /* List
             /@ Complement[Keys @ intermediate, {"TimeZoneString"}]];
        If[(* if there's a year component and it's 0 or less *)KeyExistsQ[
            intermediate, "Year"] && intermediate["Year"] <= 0,
                     (* then subtract 1 from it (because in ISO 8601 
                format, year
                 0000 means 1 BCE) *)
            intermediate["Year"] -= 1
        ];
        intermediate["TimeZone"] =
            If[     (* if there's a time zone suffix string (e.g. "+08:00"
                
                 or "Z") specified *)KeyExistsQ[intermediate, "TimeZoneString"
    ],
                (* then parse it to a UTC offset number *)
                parseTimeZone[intermediate["TimeZoneString"], OptionValue[
                    TimeZone]]
                ,
                (* else use the value of the TimeZone option *)
                OptionValue[TimeZone]
            ];
        If[(* if the parsing of the time zone suffix string failed *)
            FailureQ @ intermediate["TimeZone"],
                  (* then issue a message and fallback to the value of
                 the 
                TimeZone option *)
            (
                Message[FromISOTimestamp::invtz, intermediate["TimeZoneString"
                    ], OptionValue[TimeZone]];
                intermediate["TimeZone"] = OptionValue[TimeZone]
            )
        ];
        If[(* if the second is fractional *)KeyExistsQ[intermediate, 
            "SecondFraction"],
            (* then add the fractional and integer parts *)
            intermediate["Second"] += intermediate["SecondFraction"]
        ];
        If[(* if there's an ISO week number specified *)KeyExistsQ[intermediate,
             "WeekNumber"],
            (
                (* then convert it into an ordinal day number *)(* formula
                    
                     from https://en.wikipedia.org/wiki/ISO_week_date
     *)yearJan4 = DateObject[{intermediate["Year"], 1, 4}, TimeZone -> intermediate[
    "TimeZone"]];
                intermediate["WeekDayExact"] = Lookup[intermediate, "WeekDay",
                     1];
                intermediate["OrdinalDay"] = intermediate["WeekNumber"
                    ] * 7 + intermediate["WeekDayExact"] - (DateValue[yearJan4, "ISOWeekDay"
                    ] + 3);
            )
        ];
               (* this is the order in which date components will be 
            supplied
             to DateObject[{...}] *)
               (* strings in here get replaced with the corresponding
             value 
            from the `intermediate` association; other values (e.g. numbers
    ) go into
             DateObject verbatim *)
        lookupOrder =
            Which[
                KeyExistsQ[intermediate, "OrdinalDay"],
                    {"Year", 1, "OrdinalDay", "Hour", "Minute", "Second"
                        }
                ,
                True,
                    {"Year", "Month", "Day", "Hour", "Minute", "Second"
                        }
            ];
                    (* replace strings with the corresponding value from
             `intermediate`
            , using Null if the key doesn't exist *)
        dateSpec = lookupOrder /. key_String :> Lookup[intermediate, key,
             Null];
                (* trim Nulls from the end of the list (so that DateObject
             can
             autodetect the granularity *)
        dateSpec = Drop[dateSpec, -LengthWhile[Reverse @ dateSpec, # 
            === Null&]];
                    (* replace any remaining Nulls with 0 (i.e. if year
             and hour 
            are specified but not month or day ) *)
        dateSpec = dateSpec /. Null -> 0;
        If[(* if there's a second specified and no fraction *)And[KeyExistsQ[
            intermediate, "Second"], KeyFreeQ[intermediate, "SecondFraction"], dateObjectGranularity
             === Automatic],
                   (* then default to Second granularity (instead of 
                DateObject
                 default, Instant) *)
            dateObjectGranularity = "Second"
        ];
        If[(* if there's no year specified *)KeyFreeQ[intermediate, "Year"
            ],
                    (* then it's a time-only string, so parse it as a
                 TimeObject
                 and return that *)
            Return @ TimeObject[Drop[dateSpec, 3], dateObjectGranularity,
                 TimeZone -> intermediate["TimeZone"]];
        ];
        (* create a DateObject using automatic granularity *)
        dateObject = DateObject[dateSpec, TimeZone -> intermediate["TimeZone"
            ]];
        If[  (* if the date is specified in week-only format (e.g. "2013W06"
            
            ) *)And[KeyExistsQ[intermediate, "WeekNumber"], KeyFreeQ[
    intermediate, "WeekDay"], dateObjectGranularity === Automatic, dateObject[
    "Granularity"] === "Day"],
            (* then change granularity to Week *)
            dateObjectGranularity = "Week";
        ];
              (* apply the user-specified granularity (or Automatic) 
            to the
             DateObject *)
        Return @ DateObject[dateObject, dateObjectGranularity, CalendarType
             -> OptionValue[CalendarType], DateFormat -> OptionValue[DateFormat]]
    ]

FromISOTimestamp[strs : {__String}, granularity_:Automatic, opts : OptionsPattern[
    ]] :=
    FromISOTimestamp[#, granularity, opts]& /@ strs



HoldPattern[Options[EasterSunday]] ^:=
    Replace[ToExpression["Calendar`EasterSunday", InputForm, HoldComplete
        ], HoldComplete[sym_] :> Options[sym]]

HoldPattern[SetOptions[EasterSunday, opts___]] ^:=
    Replace[ToExpression["Calendar`EasterSunday", InputForm, HoldComplete
        ], HoldComplete[sym_] :> SetOptions[sym, opts]]

EasterSunday[args___] :=
    (
        Block[{$ContextPath},
            Needs["Calendar`"]
        ];
        Quiet[DateObject[Symbol["Calendar`EasterSunday"] @@ HoldComplete[
            args]], {General::obsfun}]
    )

EasterSunday[] :=
    EasterSunday[DateValue["Year"]]
    
  


HoldPattern[Options[EasterSundayGreekOrthodox]] ^:=
    Replace[ToExpression["Calendar`EasterSundayGreekOrthodox", InputForm,
         HoldComplete], HoldComplete[sym_] :> Options[sym]]

HoldPattern[SetOptions[EasterSundayGreekOrthodox, opts___]] ^:=
    Replace[ToExpression["Calendar`EasterSundayGreekOrthodox", InputForm,
         HoldComplete], HoldComplete[sym_] :> SetOptions[sym, opts]]

EasterSundayGreekOrthodox[args___] :=
    (
        Block[{$ContextPath},
            Needs["Calendar`"]
        ];
        Quiet[DateObject[Symbol["Calendar`EasterSundayGreekOrthodox"]
             @@ HoldComplete[args]], {General::obsfun}]
    )

EasterSundayGreekOrthodox[] :=
    EasterSundayGreekOrthodox[DateValue["Year"]]


(*LifetimeChart // ClearAll;

Options[LifetimeChart] = {"Granularity" \[Rule] "Week", "Grouped" \[Rule] False, 
    "Shape" \[Rule] "Square", "PastStyle" \[Rule] Automatic, "FutureStyle" \[Rule] Automatic,
     LabelStyle \[Rule] Automatic, AxesLabel \[Rule] Automatic, Appearance \[Rule] "Vertical",
     Background \[Rule] White, Magnification \[Rule] 1, Spacings \[Rule] Automatic, BaseStyle
     \[Rule] Automatic};

LifetimeChart[opts : OptionsPattern[]] :=
    LifetimeChart[0, opts]

LifetimeChart[a_, opts : OptionsPattern[]] :=
    Module[{age = a, items, shape, granularity, pastStyle, futureStyle,
         labelStyle, axesLabels, spacingFactor, bs},
        If[NumericQ @ age,
            age = Quantity[age, "Year"]
        ];
        If[QuantityQ[age] && CompatibleUnitQ[Quantity[1, "Year"], age
            ],
            age = N @ First @ UnitConvert[age, OptionValue @ "Granularity"
                ]
            ,
            Return[$Failed, Module]
        ];
        granularity = OptionValue @ "Granularity" /. Automatic \[Rule] "Week"
            ;
        If[!MemberQ[{"Week", "Month", "Year"}, granularity],
            Return[$Failed, Module]
        ];
        pastStyle = OptionValue @ "PastStyle" /. Automatic \[Rule] FaceForm
             @ Gray;
        futureStyle = OptionValue @ "FutureStyle" /. Automatic \[Rule] FaceForm
             @ White;
        labelStyle = OptionValue @ LabelStyle /. Automatic \[Rule] Directive[
            FontSize \[Rule] 30, FontColor \[Rule] Black];
        axesLabels = OptionValue @ AxesLabel /. {Automatic \[Rule] {Style[ToUpperCase[
            granularity <> "s"], labelStyle], Style["YEARS", labelStyle]}, None \[Rule]
             {}};
        spacingFactor = OptionValue @ Spacings /. Automatic \[Rule] 1;
        bs = OptionValue @ BaseStyle;
        shape =
            Switch[OptionValue @ "Shape",
                Automatic | "Square" | Square | "Rectangle" | Rectangle,
                    
                    Rectangle[]
                ,
                "Disk" | "Disc" | Disk | Circle | "Circle",
                    Disk[]
                ,
                _,
                    Return[$Failed, Module];
            ];
        Switch[granularity,
            "Week",
                items =
                    Table[
                        Graphics[
                            {
                                EdgeForm[{Black, Thick}]
                                ,
                                If[w \[LessEqual] age,
                                    pastStyle
                                    ,
                                    futureStyle
                                ]
                                ,
                                shape
                            }
                            ,
                            ImageSize \[Rule] {30, 30}
                        ]
                        ,
                        {w, 52 * 90}
                    ];
                items = Partition[items, 52];
                items =
                    Prepend[
                        items
                        ,
                        Table[
                            If[Mod[i, 4] \[Equal] 3,
                                Style[ToString[i + 1], labelStyle]
                                ,
                                If[Mod[i, 4] \[Equal] 0,
                                    SpanFromLeft
                                    ,
                                    ""
                                ]
                            ]
                            ,
                            {i, 52}
                        ]
                    ];
                items =
                    ResourceFunction["PrependColumn"][
                        items
                        ,
                        Prepend[
                            Table[
                                If[Mod[i, 5] \[Equal] 4,
                                    Style[ToString[i + 1], labelStyle
                                        ]
                                    ,
                                    If[Mod[i, 5] \[Equal] 0,
                                        SpanFromAbove
                                        ,
                                        ""
                                    ]
                                ]
                                ,
                                {i, 90}
                            ]
                            ,
                            ""
                        ]
                    ];
            ,
            "Month",
                items =
                    Table[
                        Graphics[
                            {
                                EdgeForm[{Black, Thick}]
                                ,
                                If[w \[LessEqual] age,
                                    pastStyle
                                    ,
                                    futureStyle
                                ]
                                ,
                                shape
                            }
                            ,
                            ImageSize \[Rule] {30, 30}
                        ]
                        ,
                        {w, 12 * 90}
                    ];
                items = Partition[items, 36];
                items =
                    Prepend[
                        items
                        ,
                        Table[
                            If[Mod[i, 4] \[Equal] 3,
                                Style[ToString[i + 1], labelStyle]
                                ,
                                If[Mod[i, 4] \[Equal] 0,
                                    SpanFromLeft
                                    ,
                                    ""
                                ]
                            ]
                            ,
                            {i, 36}
                        ]
                    ];
                items =
                    ResourceFunction["PrependColumn"][
                        items
                        ,
                        Prepend[
                            Table[
                                If[Mod[i, 5] \[Equal] 4,
                                    Style[ToString[(3 i + 3)], labelStyle
                                        ]
                                    ,
                                    If[Mod[i, 5] \[Equal] 0,
                                        SpanFromAbove
                                        ,
                                        ""
                                    ]
                                ]
                                ,
                                {i, 30}
                            ]
                            ,
                            ""
                        ]
                    ];
            ,
            "Year",
                items =
                    Table[
                        Graphics[
                            {
                                EdgeForm[{Black, Thick}]
                                ,
                                If[w \[LessEqual] age,
                                    pastStyle
                                    ,
                                    futureStyle
                                ]
                                ,
                                shape
                            }
                            ,
                            ImageSize \[Rule] {30, 30}
                        ]
                        ,
                        {w, 90}
                    ];
                items = Partition[items, 10];
                items =
                    Prepend[
                        items
                        ,
                        Table[
                            If[Mod[i, 5] \[Equal] 4,
                                Style[ToString[i + 1], labelStyle]
                                ,
                                If[Mod[i, 5] \[Equal] 0,
                                    SpanFromLeft
                                    ,
                                    ""
                                ]
                            ]
                            ,
                            {i, 10}
                        ]
                    ];
                items =
                    ResourceFunction["PrependColumn"][
                        items
                        ,
                        Prepend[
                            Table[
                                Switch[i,
                                    5 | 9,
                                        SpanFromAbove
                                    ,
                                    8,
                                        Style["90", labelStyle]
                                    ,
                                    4,
                                        Style["50", labelStyle]
                                    ,
                                    _,
                                        ""
                                ]
                                ,
                                {i, 9}
                            ]
                            ,
                            ""
                        ]
                    ];
            ,
            _,
                Return[$Failed, Module];
        ];
        If[MemberQ[{"Vertical", Automatic, Vertical}, OptionValue @ Appearance
            ],
            Magnify[
                Framed[
                    Labeled[
                        Grid[
                            items
                            ,
                            Spacings \[Rule]
                                If[!OptionValue @ "Grouped",
                                    .5 spacingFactor
                                    ,
                                    {{.5, .5, spacingFactor * {0.5, 0.5,
                                         0.5, 2}}, {.5, .5, spacingFactor * {.5, .5, .5, .5, .5, .5, .5, .5, 
                                        .5, 2}}}
                                ]
                            ,
                            ItemSize \[Rule] Automatic
                            ,
                            Alignment \[Rule] {Right, Bottom}
                            ,
                            BaseStyle \[Rule] bs
                        ]
                        ,
                        axesLabels
                        ,
                        {{Top, Center}, {Left, Center}}
                        ,
                        Spacings \[Rule] {2, 2}
                        ,
                        BaseStyle \[Rule] bs
                    ]
                    ,
                    FrameStyle \[Rule] None
                    ,
                    FrameMargins \[Rule] {{10, 20}, {20, 10}}
                    ,
                    Background \[Rule] OptionValue[Background]
                    ,
                    BaseStyle \[Rule] bs
                ]
                ,
                OptionValue @ Magnification
            ]
            ,
            Magnify[
                Framed[
                    Labeled[
                        Grid[
                            Transpose[items] /. {SpanFromLeft \[Rule] SpanFromAbove,
                                 SpanFromAbove \[Rule] SpanFromLeft}
                            ,
                            Spacings \[Rule]
                                If[!OptionValue @ "Grouped",
                                    .5 spacingFactor
                                    ,
                                    {{.5, .5, spacingFactor * {.5, .5,
                                         .5, .5, .5, .5, .5, .5, .5, 2}}, spacingFactor * {.5, .5, {.5, .5, .5,
                                         2}}}
                                ]
                            ,
                            ItemSize \[Rule] Automatic
                            ,
                            Alignment \[Rule] {Right, Bottom}
                            ,
                            BaseStyle \[Rule] bs
                        ]
                        ,
                        Reverse @ axesLabels
                        ,
                        {{Top, Center}, {Left, Center}}
                        ,
                        Spacings \[Rule] {2, 2}
                        ,
                        BaseStyle \[Rule] bs
                    ]
                    ,
                    FrameStyle \[Rule] None
                    ,
                    FrameMargins \[Rule] {{10, 20}, {20, 10}}
                    ,
                    Background \[Rule] OptionValue[Background]
                    ,
                    BaseStyle \[Rule] bs
                ]
                ,
                OptionValue @ Magnification
            ]
        ]
    ]*)


(*ClockHandsAngles // ClearAll;

ClockHandsAngles[args___] :=
    Module[{res},
        update[];
        res = Symbol["ResourceFunctionHelpers`ClockHandsAngles"][args
            ];
        res /; Head @ res =!= Symbol["ResourceFunctionHelpers`ClockHandsAngles"
            ]
    ];

update // ClearAll;

update[] :=
    Once[
        PacletManager`PacletUpdate["ResourceFunctionHelpers", "Site" 
            -> "http://pacletserver.wolfram.com", "UpdateSites" -> True];
        Quiet @
            Block[{$ContextPath},
                Get["ResourceFunctionHelpers`"]
            ]
    ];*)


(*RelativeTimeString // ClearAll;
$inDef = False;
$debug = True;
beginDefinition // ClearAll;
beginDefinition // Attributes = { HoldFirst };
beginDefinition::Unfinished =
"Starting definition for `1` without ending the current one.";
beginDefinition[ s_Symbol ] /; $debug && $inDef :=
    WithCleanup[
        $inDef = False
        ,
        Print @ TemplateApply[ beginDefinition::Unfinished, HoldForm @ s ];
        beginDefinition @ s
        ,
        $inDef = True
    ];
    beginDefinition[ s_Symbol ] :=
    WithCleanup[ Unprotect @ s; ClearAll @ s, $inDef = True ];
    
    endDefinition // beginDefinition;
    endDefinition // Attributes = { HoldFirst }*)


(* ::Section::Closed:: *)
(*Package Footer*)


End[];
EndPackage[];
