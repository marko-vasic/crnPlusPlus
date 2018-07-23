(* ::Package:: *)

(* ::Text:: *)
(* Utils by Marko Vasic *)


(* ::Section:: *)
(*Public interface specification*)

Needs["CRNSimulator`"]
BeginPackage["utils`",{"CRNSimulator`"}];

PhasifyRsys::usage = "Phasifies rsys by adding oscillator species as catalysts"

MakeOscillatorSpecies::usage = "Creates a CRN that implements the oscillatory behavior. \
Arguments: \
  phases_Integer - number of phases of the oscillator. \
  concentration_ - concentration of the oscillator species \
  speed_ - the reaction speed constant of the oscillator species";

GetOscillatorSpeciesName::usage = "Get a name of the oscillator species for a given phase number"

SimulateAndPlotRxnSys::usage = "Simulates the reaction system and plots the results. \
Arguments: \
  rsys_List - represents the reaction system \
  speciesToPlot_List - a list of species that are to be plot. \
  tmax_ - the end time of the simulation - results will be simulated for t=[0,tmax]";

PlotRxnSys::usage = "Plots the simulation results of the reaction system for all t present in sol \
Arguments: \
  sol_ - Contains results of simulation to be plot. \
  speciesToPlot_List - represent a List of species that are to be plot.";

PlotRxnSys::usage = "Plots the simulation results of the reaction system \
Arguments: \
  sol_ - Contains results of simulation to be plot \
  speciesToPlot_List - represent a List of species that are to be plot. \
  tmax_ - plot results for range t=[0,tmax]. Error if the Not does not contain results for that range of t.";

EvaluateRxnAtPoint::usage = " Evaluates a value of the reaction at a given point \
Arguments: \
  sol_ - Rxn simulation results \
  species_ - Species \
  t_ - Point in time";

GenerateAbsenceIndicator::usage = "Generates a set of reactions producing absence indicator species \
for a given species; i.e. absence indicator species should be present when given species are not. \
Arguments: \
  species_ - Name of species for which absence indicator is generated \
  rs_ - Rate of producing absence species \
  rf_ - Rate of consuming absence species in presence of target species \
Generally rf/rs should be high, to ensure that absence species are present when target species are not.";
GetAbsenceSpeciesName::usage = "Gets the name of absence species for a given target species. \
pTODO: Not the most convenient way to usage, and we can think of the better way.";

GenerateDimerizedAbsenceIndicator::usage = "Similar to GenerateAbsenceIndicator, but absence species \
are consumed at a significantly slower rate. It is better indicator of 'absence' but is more slowly generated.";

GetDimerizedAbsenceSpeciesName::usage = "Gets the name of dimerized absence species for a given target species. \
TODO: Not the most convenient way to usage, and we can think of the better way.";

TriggerIfAbsent::usage = "Adds absence indicator into the reaction such that it simulates \
reaction happening only if given species are absent. \
Arguments: \
  species_ - Species that should be absent of reaction to occur. \
  rxns_ - System of reactions that should be triggered only on absence.";

TriggerIfDimerizedAbsent::usage = "Similar to TriggerIfAbsent, but uses Dimerized Absence Indicator";

TriggerIfPresent::usage = "Reaction occurs in presence of predefined catalysts species \
Arguments: \
  catalystsSpecies_ - Catalysts species \
  rxns_ - Rxn system that should react only in a presence of catalystsSpecies";

srxn::usage = "Represents a reaction in the following form: srxn[[reaction index],{x1,x2},{x3},k] format ('structured reaction format').";

flux::usage = "Represents flux variables.";

IsMinFunc::usage = "Heuristic check if species implement max function"

IsMaxFunc::usage = "Heuristic check if species implement min function"

Begin["`Private`"];

MakeOscillatorSpecies[phases_Integer,concentration_:1,speed_:1] :=
    (
        (** note, every 3rd species is exclusive with each other **)
        Sequence@@Flatten[
        {
            conc[{x[1],x[2]},concentration],
            conc[Table[x[i],{i,3,3*phases}],10^-10],
            Table[rxn[x[i]+x[i+1],2x[i+1],speed],{i,1,3*phases-1}],
            rxn[x[3*phases]+x[1],2x[1],speed]
        }]
    )

GetOscillatorSpeciesName[phase_] :=
    (
        Return[x[phase*3]];
    )
    
PhasifyRsys[rsys_] :=
    (
        Module[ {
              rxnsPerPhase,
              rxnsPhasified,
              numberOfPhases,
              phaseNum,
              currentPhaseRxns,
              currentPhaseRxnsPhasified,
              rsysWithoutPhases,
              finalRxns
              },
            rxnsPerPhase = Cases[rsys, Global`phase[rxns_] -> rxns];
            numberOfPhases = Length[rxnsPerPhase];
            rxnsPhasified = List[];
            For[phaseNum = 1, phaseNum <= Length[rxnsPerPhase], phaseNum++,
               currentPhaseRxns = rxnsPerPhase[[phaseNum]];
               currentPhaseRxnsPhasified = 
               currentPhaseRxns /. 
               rxn[rs_, ps_, k_] -> 
                rxn[Evaluate[x[3*phaseNum] + rs], 
                 Evaluate[x[3*phaseNum] + ps], k];
               rxnsPhasified = Append[rxnsPhasified, currentPhaseRxnsPhasified];
             ];
            rsysWithoutPhases = DeleteCases[rsys, Global`phase[rxns_]];
            finalRxns = Flatten[Append[rsysWithoutPhases, rxnsPhasified]];
            finalRxns = Flatten[Append[finalRxns,{MakeOscillatorSpecies[numberOfPhases]}]];
            Return[finalRxns];
        ]
    )

SimulateAndPlotRxnSys[rsys_List, speciesToPlot_List, tmax_] :=
    Module[ {sol, species, plotter},
        (
            sol = SimulateRxnsys[rsys, tmax];
            (** add [t] to every species, needed for plotting **)
            species = Cases[speciesToPlot,x_->x[t]];
            plotter = species/.sol;
            Plot[plotter,{t,0,tmax},PlotRange->{0,All}]
        )
    ]

PlotRxnSys[sol_, speciesToPlot_List, tmax_] :=
    Module[ {species, plotter},
        (
            (** add [t] to every species, needed for plotting **)
            species = Cases[speciesToPlot,x_->x[t]];
            plotter = species/.sol;
            Return[Plot[plotter,{t,0,tmax},PlotRange->{0,All}]];
        )
    ]

PlotRxnSys[sol_, speciesToPlot_List] :=
    Module[ {tmax, plot},
        (
            (** a crazy way to get Domain range **)
            tmax = ((List@@sol[[1]])[[2]])["Domain"][[1]][[2]];
            plot = PlotRxnSys[sol, speciesToPlot, tmax];
            Return[plot];
        )
    ]

EvaluateRxnAtPoint[sol_,species_,t_] :=
    Module[ {plotter, fun},
        (
            plotter = {species}/.sol;
            fun = plotter[[1]];
            Return[fun[t]];
        )
    ]

GenerateAbsenceIndicator[species_, rs_ : 1, rf_ : 1000] :=
    Module[ {tmpSpecies, absenceIndicatorSpecies},
        (
            tmpSpecies = StringForm["`1``2`", species, "AbsenceGenerator"];
            absenceIndicatorSpecies = StringForm["`1``2`", species, "AbsenceIndicator"];
            Sequence@@Flatten[
            {
                conc[tmpSpecies, 1],
                rxn[tmpSpecies, absenceIndicatorSpecies + tmpSpecies, rs],
                rxn[species + absenceIndicatorSpecies, species, rf]
                (* in paper there is third reaction as well, but seems they do not use it. Why? *)
                (*rxn[absenceIndicatorSpecies + absenceIndicatorSpecies, absenceIndicatorSpecies, rf]*)
            }]
        )
    ]

GetAbsenceSpeciesName[species_] :=
    (
        StringForm["`1``2`", species, "AbsenceIndicator"]
    )

TriggerIfAbsent[species_,rxns_] :=
    Module[ {absenceSpecies,i,absenceExpression},
        (
            If[ ToString[Head[species]] != "List",
                 (* if species is symbol *)
                absenceExpression = GetAbsenceSpeciesName[species],
                 (* if species is list *)
                For[i = 1, i <= Length[species], i++,
                      absenceSpecies = GetAbsenceSpeciesName[species[[i]]];
                      If[ i == 1,
                          absenceExpression = absenceSpecies;,
                          absenceExpression = Evaluate[absenceExpression + absenceSpecies];
                      ]; 
                 ];
            ];
            Sequence@@Flatten[rxns/.rxn[rs_,ps_,k_]->rxn[Evaluate[absenceExpression + rs],Evaluate[absenceExpression + ps],k]]
        )
    ]

GenerateDimerizedAbsenceIndicator[species_, rs_ : 1, rf_ : 1000] :=
    Module[ {tmpSpecies, absenceIndicatorSpecies, dimerizedAbsenceIndicatorSpecies},
        (
            tmpSpecies = StringForm["`1``2`", species, "AbsenceGenerator"];
            absenceIndicatorSpecies = StringForm["`1``2`", species, "AbsenceIndicator"];
            dimerizedAbsenceIndicatorSpecies = StringForm["`1``2`", species, "DimerizedAbsenceIndicator"];
            Sequence@@Flatten[
            {
                conc[tmpSpecies, 1],
                rxn[tmpSpecies, absenceIndicatorSpecies + tmpSpecies, rs],
                rxn[species + absenceIndicatorSpecies, species, rf],
                revrxn[2*absenceIndicatorSpecies, dimerizedAbsenceIndicatorSpecies, rs, rf]
            }]
        )
    ]

GetDimerizedAbsenceSpeciesName[species_] :=
    (
        StringForm["`1``2`", species, "DimerizedAbsenceIndicator"]
    )

TriggerIfDimerizedAbsent[species_,rxns_] :=
    Module[ {absenceSpecies,
        i,
        absenceExpression},
        (
             If[ ToString[Head[species]] != "List",
                 (* if species is symbol *)
                 absenceExpression = GetDimerizedAbsenceSpeciesName[species],
                 (* if species is list *)
                 For[i = 1, i <= Length[species], i++,
                       absenceSpecies = GetDimerizedAbsenceSpeciesName[species[[i]]];
                       If[ i == 1,
                           absenceExpression = absenceSpecies;,
                           absenceExpression = Evaluate[absenceExpression + absenceSpecies];
                       ]; 
                  ];
             ];
             Sequence@@Flatten[rxns/.rxn[rs_,ps_,k_]->rxn[Evaluate[absenceExpression + rs],Evaluate[absenceExpression + ps],k]]
        )
    ]

TriggerIfPresent[catalystsSpecies_,rxns_] :=
    (
        Sequence@@Flatten[rxns/.rxn[rs_,ps_,k_]->rxn[Evaluate[catalystsSpecies+rs],Evaluate[catalystsSpecies+ps],k]]
    )

IsMaxFunc[sol_, a_, b_, max_] :=
    Module[ {tmax, a0, b0, maxValue},
        a0 = EvaluateRxnAtPoint[sol,a,0];
        b0 = EvaluateRxnAtPoint[sol,b,0];
        tmax = ((List@@sol[[1]])[[2]])["Domain"][[1]][[2]];
        maxValue = EvaluateRxnAtPoint[sol,max,tmax];
        If[ Abs[maxValue-Max[a0, b0]]<0.1,
            Print["***** Max[" <> ToString[a] <> "," <> ToString[b] <> "]=" <> ToString[max]],
            Null
        ];
    ]

IsMinFunc[sol_, a_, b_, min_] :=
    Module[ {tmax, a0, b0, minValue},
        a0 = EvaluateRxnAtPoint[sol,a,0];
        b0 = EvaluateRxnAtPoint[sol,b,0];
        tmax = ((List@@sol[[1]])[[2]])["Domain"][[1]][[2]];
        minValue = EvaluateRxnAtPoint[sol,min,tmax];
        If[ Abs[minValue-Min[a0, b0]]<0.1,
            Print["***** Min[" <> ToString[a] <> "," <> ToString[b] <> "]=" <> ToString[min]],
            Null
        ];
    ]

(** Private **)
End[];

EndPackage[];
