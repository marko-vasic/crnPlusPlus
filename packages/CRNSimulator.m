(* ::Package:: *)

(* ::Text:: *)
(*Chemical Reaction Network (CRN) Simulator package is developed by David Soloveichik. Copyright 2009-2021. *)
(*http://users.ece.utexas.edu/~soloveichik/crnsimulator.html*)
(**)
(*ver 2.1 (Jan 8 2022)*)
(**)
(*- Removed dependency on the Notation package and switched to Format built-in functionality. (This was in part motivated by changes in behavior of AddInputAlias with Mathematica 13.) *)
(*rxn and revrxn expressions in StandardForm are shown as standard chemical arrow notation. (Note that FullForm still preserves rxn and revrxn so all pattern matching works.) The shown arrow expressions cannot be modified but can be copied and pasted (this is done via Interpretation). *)
(**)
(*ver 2.0 (Mar 16 2021)*)


(* ::Text:: *)
(*Changes since last version:*)
(**)
(*- Allows wider range of expressions for species names including such things as a[x][y] and a[x+y].*)
(**)
(*- Introduces rxnl[] statements as an alternative way to express reactions with a list of reactants and products. For example,  rxnl[{a,b,a},{a,b},1], rxnl[{},{a},1]. Functions RxnsToRxnls and RxnlsToRxns convert between the "2a+b" and "{a,a,b}" representations.*)
(**)
(*- The order of species in rxn[] and revrxn[] statements is not preserved. In other words, rxn[b+a,a,1] is the same as rxn[a+b,a,1], and in fact if you type the first it will output the second. If you want to preserve the order syntactically for whatever reason you should use the rxnl[] statements. Note that this change might break some backward compatibility. *)
(**)
(*- Does some basic syntax error checking: eg if you type revrxn[] with just one rate constant it will spot this. It checks for the correct number of arguments for all statements and that rxnl contains lists for reactions and products.*)
(**)
(*- Allows an alternative way of defining initial values via "x[0]==4". If you have both conc[x, ...] statements and a x[0]==0 statement, then the conc[x, ...] statements are ignored. *)
(**)
(*- revrxn[] does not automatically get expanded to two rxn[] statements, and conc[{a,b},1] does not automatically get expanded to conc[a,1], conc[b,1].  Note that this change might break some backward compatibility. The idea is that if you typed revrxn[] or conc[{},...] you want to keep it that way syntactically. After all you can think of a reversible reaction as a specific kind of object that you may want to manipulate separately. There are now explicit commands ExpandRevrxns and ExpandConcs which do the expansion. (Of course, all the regular commands take either the original or expanded forms so you don't need to manually call these functions.)*)


(* ::Section:: *)
(*Public interface specification*)


BeginPackage["CRNSimulator`"];


rxn::usage="Represents an irreversible reaction. eg. rxn[a+b,c,1]";
revrxn::usage="Represents a reversible reaction. eg. revrxn[a+b,c,1,1]";
rxnl::usage="Represents a reaction in which reactants and products are represented as \
lists. Unlike the \"+\" notation, this preserves the order of reactants and products. \
eg. rxnl[{a,a,b},{a,b},1], rxnl[{},{a},1]. Functions RxnsToRxnls and RxnlsToRxns \
convert between the rxnl[] notation and the rxn[] notation.";
conc::usage="Initial concentration: conc[x,10] or conc[{x,y},10]. \
Multiple conc[] statements for the same species are added up.";
term::usage="Represents an additive term in the ODE for species x. \
Species concentrations must be expressed in x[t] form. eg. term[x, -2 x[t]*y[t]]";


SimulateRxnsys::usage=
"SimulateRxnsys[rxnsys,endtime] simulates the reaction system rxnsys for time 0 \
to endtime. In rxnsys, initial concentrations are specified by conc[] statements. \
Multiple conc[] statements for the same species are added up. \
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
with initial conditions. The format of rxnsys is described in the documentation for \
SimulateRxnsys. The time variable is given as the second argument; if omitted it is \
set to Global`t.";
RxnsToRxnls::usage=
"Converts chemistry reaction representation to list reaction representation: eg rxn[x1+x2,x3,k] to rxn[{x1,x2},{x3},k].";
RxnlsToRxns::usage=
"Converts list reaction representation to chemistry reaction representation: eg rxn[{x1,x2},{x3},k] to rxn[x1+x2,x3,k].";
ExpandConcs::usage=
"Expand conc[{a,b},1] to conc[a,1], conc[b,1].";
ExpandRevrxns::usage=
"Converts revrxn statements into a pair of rxn statements.";

(*To use instead of Sequence in functions with Hold attribute but not HoldSequence,
like Module, If, etc*)
Seq:=Sequence 

(* For debugging to export private symbols: 
ProcessRxnlToTerms::usage="";
ProcessTermsToOdes::usage="";
ExpandConcs::usage="";
MissingODEQ::usage="";
InitialValueSetQ::usage="";
*)


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* In StandardForm show rxn and revrxn using standard chemical arrow notation.
(Note that FullForm still preserves rxn and revrxn so all pattern matching works.)
The shown arrow expressions cannot be modified but can be copied and pasted.*)
Format[rxn[rs_,ps_,k_],StandardForm]:=Interpretation[Row[{rs,Overscript[RawBoxes["\[LongRightArrow]"],k],ps}," "],rxn[rs,ps,k]]
Format[revrxn[rs_,ps_,k1_,k2_],StandardForm]:=Interpretation[Row[{rs,Underoverscript[RawBoxes["\[LongLeftRightArrow]"],k2,k1],ps}," "],revrxn[rs,ps,k1,k2]]


(* Does basic check to catch common syntax errors *)
RxnsysToOdesys::rxnargs = "`1` is invalid as 3 arguments are expected";
RxnsysToOdesys::revrxnargs = "`1` is invalid as 4 arguments are expected";
RxnsysToOdesys::termargs = "`1` is invalid as 2 arguments are expected";
RxnsysToOdesys::concargs = "`1` is invalid as 2 arguments are expected";
RxnsysToOdesys::rxnlargs = "`1` is invalid as 3 arguments are expected";
RxnsysToOdesys::rxnllists = "`1` is invalid as lists are expected for the first and second arguments";
CheckSyntaxErrors[rxnsys_]:=
 If[FailureQ[
  Check[
   Message[RxnsysToOdesys::rxnargs, #]&  /@  Cases[Cases[rxnsys, rxn[___]], Except[rxn[_,_,_]]];
   Message[RxnsysToOdesys::revrxnargs, #]&  /@  Cases[Cases[rxnsys, revrxn[___]], Except[revrxn[_,_,_,_]]];
   Message[RxnsysToOdesys::termargs, #]&  /@  Cases[Cases[rxnsys, term[___]], Except[term[_,_]]];
   Message[RxnsysToOdesys::concargs, #]&  /@  Cases[Cases[rxnsys, conc[___]], Except[conc[_,_]]];
   Message[RxnsysToOdesys::rxnlargs, #]&  /@  Cases[Cases[rxnsys, rxnl[___]], Except[rxnl[_,_,_]]];
   Message[RxnsysToOdesys::rxnllists, #]&  /@  Cases[Cases[rxnsys, rxnl[_,_,_]], Except[rxnl[{___},{___},_]]],
   $Failed]],
  Abort[]]


(* Expand conc[{a,b},1] to conc[a,1], conc[b,1] *)
ExpandConcs[rxnsys_]:=
 Replace[rxnsys, conc[xs_List,c_] :> Seq@@(conc[#,c]&/@xs), {1}]

(* Converts revrxn statements into a pair of rxn statements *)
ExpandRevrxns[rxnsys_]:=
 Replace[rxnsys, revrxn[r_,p_,k1_,k2_] :> Sequence[rxn[r,p,k1],rxn[p,r,k2]], {1}]

(* Converts rxn and revrxn statements to rxnl statements *)
RxnsToRxnls[rxnsys_]:=(
 CheckSyntaxErrors[rxnsys];
 Replace[ExpandRevrxns[rxnsys],
         rxn[rs_, ps_, k_] :> 
				rxnl[Replace[Replace[rs, {ss_Plus:>List@@ss,s_:>{s}}], {_Integer -> Seq[], c_Integer*s_ :> Seq @@ Table[s, {c}], s_ :> s},{1}], 
				     Replace[Replace[ps, {ss_Plus:>List@@ss,s_:>{s}}], {_Integer -> Seq[], c_Integer*s_ :> Seq @@ Table[s, {c}], s_ :> s},{1}], 
					 k],
		 {1}])

(* Converts rxnl statements to rxn statements *)
RxnlsToRxns[rxnsys_]:=(
 CheckSyntaxErrors[rxnsys];
 Replace[rxnsys,
         rxnl[rs_,ps_,k_] :> rxn[Plus@@rs,Plus@@ps,k],
         {1}])


(* Species as products or reactants in rxn, revrxn, rxnl statements, as well as defined in x'[t]== or x[t]== statements \
or term statements, or conc statements. 
Note: species are sorted (by Union[])
*)
SpeciesInRxnsys[rxnsys_]:=(
 CheckSyntaxErrors[rxnsys];
 Union[
 	Cases[RxnsToRxnls[rxnsys],rxnl[rs_,ps_,k_]:>Sequence@@Union[rs,ps]],
 	Cases[ExpandConcs[rxnsys], x_'[_]==_ | x_[_]==_ | term[x_,__] | conc[x_,_] :> x]])
SpeciesInRxnsys[rsys_,pattern_]:=Cases[SpeciesInRxnsys[rsys],pattern]
SpeciesInRxnsysStringPattern[rsys_,pattern_]:=Select[SpeciesInRxnsys[rsys],StringMatchQ[ToString[#],pattern]&]


(* Check if a species' initial value is set in a odesys. 
Returns true also if species governed by direct equation x[t]\[Equal]... (which implies initial value) *) 	
InitialValueSetQ[odesys_,x_]:=
 MemberQ[odesys,x[_]==_]

(* Check if a species is missing an ODE or a direct definition (x[t]=_) in odesys. *)
MissingODEQ[odesys_,x_,t_Symbol]:=
 !MemberQ[odesys, D[x[t],t]==_ | x[t]==_]  	

RxnsysToOdesys[rxnsysInput_,t_Symbol:Global`t]:=
 Module[{rxnsys, spcs, concsAssoc, terms, odesys, eqsFromTerms, initsFromConcs, initsFromEqs, eqsFromEqs},
  
  spcs=SpeciesInRxnsys[rxnsysInput]; (* Note: Check for syntax errors inside of SpeciesInRxnsys; don't need explicit call *)
  
  (* Standardize rxnsys by converting rxn and revrxn to rxnl, and expanding concs over lists *) 
  rxnsys = rxnsysInput // RxnsToRxnls // ExpandConcs;

  (* Extract conc statements and sum them for same species. Returns an association, eg: \[LeftAssociation]a\[Rule]-9,b\[Rule]3\[RightAssociation] 
  *)
  concsAssoc = GroupBy[Cases[rxnsys,conc[__]],First->Last,Total];
  
  (* Extract any initial values x[0]=.... If present these should override any concs[] *)
  initsFromEqs = Cases[rxnsys, _[0]==_];
  concsAssoc = KeyDrop[concsAssoc, Cases[initsFromEqs, x_[0]==_ :> x]]; 

  (* Get term[] statements from rxnl[] statements and existing term[] statements *)
  terms=Join[
    Cases[rxnsys, rxnl:rxnl[__]:>Seq@@ProcessRxnlToTerms[rxnl,t]],
    Cases[rxnsys, term[__]]];

  (* ODEs from parsing terms *)
  eqsFromTerms = ProcessTermsToOdes[terms,t]; 
  (* initial values from concsAssoc *)
  initsFromConcs = KeyValueMap[#1[0]==#2&,concsAssoc]; 
  
  (* direct definitions of x'[t]\[Equal]... or x[t]\[Equal]... *)
  eqsFromEqs = Cases[rxnsys, _'[t]==_ | _[t]==_];
  
  (* create odesys *)
  odesys = Join[eqsFromTerms, initsFromConcs, initsFromEqs, eqsFromEqs];
  
  (* if have direct definition x[t]\[Equal]... then delete any x'[t]\[Equal]... and any initial value x[0]\[Equal]... *)
  odesys = DeleteCases[odesys, Alternatives@@Cases[eqsFromEqs, x_[t]==_ :> (x'[t]==_|x[0]==_)]];
     
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

(* Create list of term[] statements from a rxnl statement 
Note: works if rs is {}. Does't work for {2a,b}, needs to be {a,a,b}
*)
ProcessRxnlToTerms[reaction:rxnl[rs_,ps_,k_],t_Symbol]:=
Module[{spcs=SpeciesInRxnsys[{reaction}], rrate, spccoeffs},
(* compute rate of this reaction *)
rrate = k * Times@@(#[t]&/@rs);
(*for each species, get a net coefficient*)
spccoeffs=Coefficient[Plus@@ps-Plus@@rs,#]& /@ spcs;
(*create term for each species*)
MapThread[term[#1,#2*rrate]&,{spcs, spccoeffs}]]


SimulateRxnsys[rxnsys_,endtime_,opts:OptionsPattern[NDSolve]]:=
 Module[{spcs=SpeciesInRxnsys[rxnsys],odesys=RxnsysToOdesys[rxnsys,Global`t]},
 Quiet[NDSolve[odesys, spcs, {Global`t,0,endtime},opts,MaxSteps->Infinity,AccuracyGoal->MachinePrecision],{NDSolve::"precw"}][[1]]]


End[];
EndPackage[];
