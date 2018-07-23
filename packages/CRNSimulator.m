(* ::Package:: *)

(* ::Text:: *)
(*Chemical Reaction Network (CRN) Simulator package is developed by David Soloveichik. Copyright 2009-2017. *)
(*http://users.ece.utexas.edu/~soloveichik/crnsimulator.html*)


(* ::Section:: *)
(*Public interface specification*)


Notation`AutoLoadNotationPalette = False;
Needs["Notation`"]
BeginPackage["CRNSimulator`", {"Notation`"}];


rxn::usage="Represents an irreversible reaction. eg. rxn[a+b,c,1]";
revrxn::usage="Represents a reversible reaction. eg. revrxn[a+b,c,1,1]";
rxnl::usage="Represents a reaction in which reactants and products are represented as \
lists. Unlike the \"+\" notation, this preserves the order of reactants and products.";
conc::usage="Initial concentration: conc[x,10] or conc[{x,y},10].";
term::usage="Represents an additive term in the ODE for species x. \
Species concentrations must be expressed in x[t] form. eg. term[x, -2 x[t]*y[t]]";


SimulateRxnsys::usage=
"SimulateRxnsys[rxnsys,endtime] simulates the reaction system rxnsys for time 0 \
to endtime. In rxnsys, initial concentrations are specified by conc statements. \
If no initial condition is set for a species, its initial concentration is set to 0. \
Rxnsys can also include term[] statements (e.g. term[x, -2 x[t]]) which are additively \
combined together with term[]s derived from rxn[] statements. \
Rxnsys can also include direct ODE definitions for some species (e.g. x'[t]==...), \
or direct definitions of species as functions of time (e.g. x[t]==...), \
which are passed to NDSolve without modification. \
Any options specified (eg WorkingPrecision->30) \
are passed to NDSolve."; 
SpeciesInRxnsys::usage=
"SpeciesInRxnsys[rxnsys] returns the species in reaction system rxnsys. \
SpeciesInRxnsys[rxnsys,pttrn] returns the species in reaction system rxnsys \
matching Mathematica pattern pttrn (eg x[1,_]).";
SpeciesInRxnsysStringPattern::usage=
"SpeciesInRxnsysPattern[rxnsys,pttrn] returns the species in reaction system rxnsys \
matching Mathematica string pattern pttrn. \
(Eg \"g$*\" matches all species names starting with \"g$\" ; \ 
can also do RegularExpression[\"o..d.\$.*\"].)";
RxnsysToOdesys::usage=
"RxnsysToOdesys[rxnsys,t] returns the ODEs corresponding to reaction system rxnsys, \
with initial conditions. If no initial condition is set for a species, its initial \
concentration is set to 0. \
The time variable is given as the second argument; if omitted it is set to Global`t.";
RxnsToRxnls::usage=
"Converts chemistry reaction representation to list reaction representation: eg rxn[x1+x2,x3,k] to rxn[{x1,x2},{x3},k].";
SolveEquilibrium::usage=
"Calculates concentrations of species at equilibrium"
RxnsysToSrxnsys::usage=
" Produces reactions in srxn[[reaction index],{x1,x2},{x3},k] format (\"structured reaction system\"). Removes any other statements. \
  reaction index is 1-based"

(*To use instead of Sequence in functions with Hold attribute but not HoldSequence,
like Module, If, etc*)
Seq:=Sequence 


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


Notation[ParsedBoxWrapper[RowBox[{"r_", " ", OverscriptBox["\[RightArrow]", RowBox[{" ", "k_", " "}]], " ", "p_", " "}]] \[DoubleLongLeftRightArrow] ParsedBoxWrapper[RowBox[{"rxn", "[", RowBox[{"r_", ",", "p_", ",", "k_"}], "]"}]]]
Notation[ParsedBoxWrapper[RowBox[{"r_", " ", UnderoverscriptBox["\[RightArrowLeftArrow]", "k2_", "k1_"], " ", "p_", " "}]] \[DoubleLongLeftRightArrow] ParsedBoxWrapper[RowBox[{"revrxn", "[", RowBox[{"r_", ",", "p_", ",", "k1_", ",", "k2_"}], "]"}]]]
AddInputAlias[ParsedBoxWrapper[RowBox[{"\[Placeholder]", " ", OverscriptBox["\[RightArrow]", RowBox[{" ", "\[Placeholder]", " "}]], " ", "\[Placeholder]", " "}]],"rxn"]
AddInputAlias[ParsedBoxWrapper[RowBox[{"\[Placeholder]", " ", UnderoverscriptBox["\[RightArrowLeftArrow]", "\[Placeholder]", "\[Placeholder]"], " ", "\[Placeholder]", " "}]],"revrxn"]



(* People are often confused about rxn[0,x,1] instead of rxn[1,x,1]. So we automatically replace any integer with 1. *)
rxn[Except[1,_Integer],ps_,k_]:=rxn[1,ps,k]




(* Standardize rxnsys for RxnsysToOdesys. This means: *)
(* convert any rxnl to rxn, 
   expand revrxns, 
   expand conc
*)
StandardizeRxnsys[rxnsys_]:=
  rxnsys // RxnlsToRxns // ExpandRevrxns // ExpandConcs

(* Expand conc[{a,b},1] to conc[a,1], conc[b,1] *)
ExpandConcs[rxnsys_]:=
 Replace[rxnsys, conc[xs_List,c_] :> Seq@@(conc[#,c]&/@xs), {1}]

(* Converts revrxn statements into a pair of rxn statements *)
ExpandRevrxns[rxnsys_]:=
 Replace[rxnsys, revrxn[r_,p_,k1_,k2_] :> Sequence[rxn[r,p,k1],rxn[p,r,k2]], {1}]

(* Converts rxn and revrxn statements to rxnl statements *)
RxnsToRxnls[rxnsys_]:=
 Replace[ExpandRevrxns[rxnsys],
         rxn[rs_, ps_, k_] :> 
				rxnl[Replace[Replace[rs, {ss_Plus:>List@@ss,s_:>{s}}], {_Integer -> Seq[], c_Integer*s_ :> Seq @@ Table[s, {c}], s_ :> s},{1}], 
				     Replace[Replace[ps, {ss_Plus:>List@@ss,s_:>{s}}], {_Integer -> Seq[], c_Integer*s_ :> Seq @@ Table[s, {c}], s_ :> s},{1}], 
					 k],
		 {1}]

(* Converts rxnl statements to rxn statements, and expands any existing revrxn statements *)
RxnlsToRxns[rxnsys_]:=
 Replace[rxnsys,
         rxnl[rs_,ps_,k_] :> rxn[Plus@@rs,Plus@@ps,k],
         {1}]


(* Species as products or reactants in rxn, revrxn, rxnl statements, as well as defined in x'[t]== or x[t]== statements \
or term statements, or conc statements. 
Note: species are sorted (by Union[])
*)
SpeciesInRxnsys[rxnsys_]:=
 Union[
 	Cases[RxnsToRxnls[rxnsys],rxnl[rs_,ps_,k_]:>Sequence@@Union[rs,ps]],
 	Cases[ExpandConcs[rxnsys], x_'[_]==_ | x_[_]==_ | term[x_,__] | conc[x_,_] :> x]]
SpeciesInRxnsys[rsys_,pattern_]:=Cases[SpeciesInRxnsys[rsys],pattern]
SpeciesInRxnsysStringPattern[rsys_,pattern_]:=Select[SpeciesInRxnsys[rsys],StringMatchQ[ToString[#],pattern]&]


(* Check if a species' initial value is set in a odesys *) 	
InitialValueSetQ[odesys_,x_]:=
 MemberQ[odesys,x[_]==_]

(* Check if a species is missing an ODE or a direct definition (x[t]=_) in odesys. *)
MissingODEQ[odesys_,x_,t_Symbol]:=
 !MemberQ[odesys, D[x[t],t]==_ | x[t]==_]  	

RxnsysToOdesys[rxnsysInput_,t_Symbol:Global`t]:=
 Module[
  {rxnsys=StandardizeRxnsys[rxnsysInput], spcs=SpeciesInRxnsys[rxnsysInput], concs, termssys, odesys, eqsFromTerms, eqsFromConcs},

  (* extract conc statements and sum them for same species *)
  concs = conc[#[[1,1]],Total[#[[;;,2]]]]&/@GatherBy[Cases[rxnsys,conc[__]],Extract[{1}]];

  (* Convert rxn[] to term[] statements *)
  termssys=rxnsys /. rxn:rxn[__]:>Seq@@ProcessRxnToTerms[rxn,t];

  (* ODEs from parsing terms *)
  eqsFromTerms = ProcessTermsToOdes[Cases[termssys,term[__]],t]; 
  (* initial values from parsing conc statements *)
  eqsFromConcs = Cases[concs,conc[x_,c_]:>x[0]==c]; 

  (* Remove term and conc statements from rxnsys and add eqs generated from them. 
     If there is a conflict, use pass-through equations *)
  odesys = DeleteCases[termssys, term[__]|conc[__]];
  odesys = Join[odesys,
                DeleteCases[eqsFromTerms, Alternatives@@(#'[t]==_& /@ Cases[odesys,(x_'[t]|x_[t])==_:>x])],
                eqsFromConcs];
     
  (* For species still without initial values, add zeros *)
  odesys = Join[odesys, #[0]==0& /@ Select[spcs, !InitialValueSetQ[odesys,#]&]];

  (* For species still without ODE or direct definition, add zero time derivative *)
  (* This can happen for example if conc is defined, but nothing else *)
  Join[odesys, D[#[t],t]==0& /@ Select[spcs, MissingODEQ[odesys,#,t]&]]
 ]
 
(* Create list of ODEs from parsing term statements. terms should be list of term[] statements. *) 
ProcessTermsToOdes[terms_,t_Symbol]:=
Module[{spcs=Union[Cases[terms,term[s_,_]:>s]]},
#'[t]==Total[Cases[terms,term[#,rate_]:>rate]] & /@ spcs];

(* Create list of term[] statements from parsing a rxn statement *) 
ProcessRxnToTerms[reaction:rxn[r_,p_,k_],t_Symbol]:=
Module[{spcs=SpeciesInRxnsys[{reaction}], rrate, spccoeffs,terms},
(* compute rate of this reaction *)
rrate = k (r/.{Times[b_,s_]:>s^b,Plus->Times});
(*for each species, get a net coefficient*)
spccoeffs=Coefficient[p-r,#]& /@ spcs;
(*create term for each species*)
terms=MapThread[term[#1,#2*rrate]&,{spcs, spccoeffs}];
(*change all species variables in the second arg in term[] to be functions of t*)
terms/.term[spc_,rate_]:>term[spc,rate/.s_/;MemberQ[spcs,s]:>s[t]]];


SimulateRxnsys[rxnsys_,endtime_,opts:OptionsPattern[NDSolve]]:=
 Module[{spcs=SpeciesInRxnsys[rxnsys],odesys=RxnsysToOdesys[rxnsys,Global`t]},
 Quiet[NDSolve[odesys, spcs, {Global`t,0,endtime},opts,MaxSteps->Infinity,AccuracyGoal->MachinePrecision],{NDSolve::"precw"}][[1]]]


RxnsysToSrxnsys[rsys_] :=
    Module[{i = 1},
        Cases[
        rsys, 
        rxn[rs_, ps_, k_] :>
        srxn[i++,
        {Unevaluated[rs] /. {Plus -> Seq, c_Integer*s_ :> Seq @@ Table[s, {c}]}},
        {Unevaluated[ps] /. {Plus -> Seq, c_Integer*s_ :> Seq @@ Table[s, {c}]}},
        k]
        ]
    ]
    
(* 
  Returns state change vectors for each reaction. The order of species is determined by Sort[SpeciesInRxnsys[rsys]]
  Example, for following reaction system
  rxn1: a -> b
  rxn2: a+c -> c
  Result is:
  {
    {-1,1,0},
    {-1,0,0}
  }
  Each row coresponds to the reaction, and elements in the row to species
*)
RxnsysToStateChangeVectors[rsys_] :=
    Module[ {
    spcs = Sort[SpeciesInRxnsys[rsys]], 
       srxnsys = RxnsysToSrxnsys[rsys],
       symbolicNetChanges
    },
        symbolicNetChanges = Cases[srxnsys, srxn[_, r_, p_, _] :> Plus @@ p - Plus @@ r];
        Outer[Coefficient[#1, #2] &, symbolicNetChanges, spcs]
    ]
    
FluxForMatchingReactants[rsys_, fluxvars_] :=
    Module[{
        equations,
        reactionsGroupedByReactants,
        reactions,
        index1,
        index2,
        reaction1,
        reaction2,
        reaction1Index,
        reaction2Index,
        reaction1Rate,
        reaction2Rate
    },
    	equations={};
		reactionsGroupedByReactants=GatherBy[
  			Cases[RxnsysToSrxnsys[rsys], srxn[index_,reactants_,_,rate_]->{reactants, index, rate}],
  			First
		];
		For[i=1, i <= Length[reactionsGroupedByReactants], i++, 
  			reactions=reactionsGroupedByReactants[[i]];
  			If[Length[reactions] > 1,
    			For[index1 = 1, index1 <= Length[reactions], index1++,
      				For[index2 = index1 + 1, index2 <= Length[reactions], index2++,
        				reaction1 = reactions[[index1]];
        				reaction2 = reactions[[index2]];
        				reaction1Index = reaction1[[2]];
        				reaction2Index = reaction2[[2]];
        				reaction1Rate = reaction1[[3]];
        				reaction2Rate = reaction2[[3]];
        				equations=Append[equations,fluxvars[[reaction1Index]]/fluxvars[[reaction2Index]]==reaction1Rate/reaction2Rate];
      				]
    			]
			]
		];
		Return[equations];
    ]
    
FluxForRevRxns[rsys_, fluxvars_] :=
    Module[{
       i,
       j,
       srxnsys = RxnsysToSrxnsys[rsys],
       reaction1,
       index1,
       reactants1,
       products1,
       reaction2,
       index2,
       reactants2,
       products2,
       equations
    },
    	equations={};
    	For[i = 1, i <= Length[srxnsys], i++,
    	    reaction1 = srxnsys[[i]];
    	    index1 = reaction1[[1]];
    	    reactants1 = reaction1[[2]];
    	    products1 = reaction1[[3]];
			
    	    For[j = i + 1, j <= Length[srxnsys], j++,
    	        reaction2 = srxnsys[[j]];
	    		index2 = reaction2[[1]];
    	    	reactants2 = reaction2[[2]];
    	    	products2 = reaction2[[3]];
    	    	If[(reactants1 == products2) && (reactants2 == products1),
					equations = Append[equations, fluxvars[[index2]] == 0];
    	    	]
    	    ]
    	];
    	Return[equations];
    ]
    
RevRxnConservationEquations[rsys_, fluxVars_] :=
	Module[ {
	    spcs = Sort[SpeciesInRxnsys[rsys]],
	    initialConcentrations,
	    reversibleReactions,
	    expandedReactions,
	    conservationEquations,
	    i,
	    equations
	},
		If[Count[rsys, revrxn[___]] == 0, Return[{}]];
		initialConcentrations = Plus @@ Cases[rsys, conc[#, c_]:>c] & /@ spcs;
  		reversibleReactions=Flatten[Cases[rsys, revrxn[reactants_,products_,k1_,k2_]->{revrxn[reactants, products, k1, k2]}]];
  		expandedReactions=ExpandRevrxns[reversibleReactions];
  		equations = {};
  		For[i = 2, i <= Length[fluxVars], i += 2,
  		    equations = Append[equations, fluxVars[[i]] == 0];
  		];
  		conservationEquations = Thread[spcs==fluxVars.RxnsysToStateChangeVectors[expandedReactions] + initialConcentrations];
  		Return[Join[equations,conservationEquations]];
	]
    
SolveEquilibrium[rsys_] :=
    Module[ {
    spcs = Sort[SpeciesInRxnsys[rsys]],
    odesys = RxnsysToOdesys[rsys],
    numrxns,
    initialConcentrations, (* initial values *)
    rates, (* reaction rates *)
    fluxvars,  (* flux[i] variables capturing flux of each reaction *)
    sseqns, (* steady state equations *)
    conservationEquations, (* conservation equations *)
    identicalReactantsEquations, (* flux equations for reactions with equal reactants *)
	concPositive,
	initConcPositive,
	fluxNonNegative,
    ratesPositive, (* inequalities enforcing positivity of reaction rates *)
    assumptions,
    sol,
    fluxForRevRxns,
    expandedRsys
    },
    	expandedRsys = ExpandRevrxns[rsys];
    	numrxns = Count[expandedRsys, rxn[___]];
        initialConcentrations = Plus @@ Cases[rsys, conc[#, c_]:>c] & /@ spcs;
        rates = List @@ Cases[expandedRsys, rxn[a_, b_, k_]:>k];
        sseqns = Cases[odesys,_'[Global`t]==rhs_:>0==(rhs/.s_[Global`t]:>s)];
        fluxvars = Table[flux[i],{i,1,numrxns}];
        conservationEquations = Thread[spcs==fluxvars.RxnsysToStateChangeVectors[expandedRsys] + initialConcentrations];
        identicalReactantsEquations = FluxForMatchingReactants[expandedRsys, fluxvars];
        fluxForRevRxns = FluxForRevRxns[expandedRsys, fluxvars];
        concPositive = #>=0&/@Join[spcs];
        initConcPositive = #>0&/@Join[initialConcentrations];
        fluxNonNegative = #>=0&/@Join[fluxvars];
        ratesPositive = #>0&/@rates;
        assumptions = Join[concPositive, initConcPositive, fluxNonNegative, ratesPositive];
        sol = Solve[
            Join[sseqns, conservationEquations, fluxForRevRxns, identicalReactantsEquations, assumptions],
            Join[spcs, fluxvars],
            Reals
        ];
        Return[Simplify[sol, assumptions]];
    ]

End[];
EndPackage[];
