(* Colors for plots *)
(* http://ksrowell.com/blog-visualizing-data/2012/02/02/optimal-colors-for-graphs/ *)

(* Medium intensity *)
GrayM = RGBColor["#737373"];
RedM = RGBColor["#EF5B63"];
GreenM = RGBColor["#7CC26E"];
BlueM = RGBColor["#5D9CD2"];
OrangeM = RGBColor["#F8A662"];
PurpleM = RGBColor["#9D69A9"];
BrickM = RGBColor["#CC705B"];
PinkM = RGBColor["#D581B3"];

(*BlueC = RGBColor[{57., 106., 177.}/256];
OrangeC = RGBColor[{218., 124., 48.}/256];
GreenC = RGBColor[{62., 150., 81.}/256];
RedC = RGBColor[{204., 37., 41.}/256];
GrayC = RGBColor[{83., 81., 84.}/256];
VioletC = RGBColor[{107., 76., 154.}/256];
MaroonC = RGBColor[{146., 36., 40.}/256];*)

LineThickness = 0.008;

PlotForPaper[functions_, tmax_, xTicksDist_ : 100, additionalOptions_ : {}] :=
  Module[{},
    Plot[functions, {t,0,tmax},
      PlotRange->{0,All},
      PlotStyle->
      {
        {GreenM, Thickness[LineThickness]},
        {OrangeM, Thickness[LineThickness]},
        {BrickM, Thickness[LineThickness]},
        {BlueM, Thickness[LineThickness]}
      },
      Ticks->{Range[0, tmax, xTicksDist],Automatic},
      BaseStyle->{FontSize->12, FontWeight->"Bold"},
      GridLines->Automatic
    ]
];