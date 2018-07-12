(* ::Section:: *)
(*Public interface specification*)

Needs["CRNSimulator`", "utils`"]
BeginPackage["primitives`",{"CRNSimulator`", "utils`"}];

ExpandDiscreteRsys::usage = "";
ExpandRsys::usage = "";
EvaluateError::usage = "Evaluates error between simulation results of rsys and intended correct values";

(* Constants *)
StepMacro = Global`step;
LdMacro = Global`Ld;
AddMacro = Global`Add;
SubMacro = Global`Sub;
MulMacro = Global`Mul;
DvdMacro = Global`Dvd;
SqrMacro = Global`Sqr;
AMMacro = Global`AM;
CmpMacro = Global`Cmp;
GtMacro = Global`IfGT;
GeMacro = Global`IfGE;
EqMacro = Global`IfEQ;
LeMacro = Global`IfLE;
LtMacro = Global`IfLT;
IfPresentMacro = Global`IfPresent;

XgtyFlag = Global`xgty;
XltyFlag = Global`xlty;
YgtxFlag = Global`ygtx;
YltxFlag = Global`yltx;

Begin["`Private`"];

NormalizeToFlagsMacro = Global`NormalizeToFlags;
FlagsAMMacro = Global`FlagsAM;
ComparisonOffset = Global`cmpOffset;

(* TODO: Delete this one, it's subset of ExpandRsys *)
ExpandDiscreteRsys[rsys_] :=
    (
        Module[ {processedRsys},
            processedRsys = rsys;
            processedRsys = StepToPhase[processedRsys];
            processedRsys = processedRsys/.LdMacro[a_,b_]:>Ld[a,b];
            processedRsys = processedRsys/.AddMacro[a_,b_,c_]:>Add[a,b,c];
            processedRsys = processedRsys/.SubMacro[a_,b_,c_]:>Sub[a,b,c];
            processedRsys = processedRsys/.MulMacro[a_,b_,c_]:>Mul[a,b,c];
            processedRsys = processedRsys/.AMMacro[x_, y_]:>AM[x, y];
            processedRsys = processedRsys/.NormalizeToFlagsMacro[x_, y_]:>NormalizeToFlags[x, y];
            processedRsys = processedRsys/.FlagsAMMacro[]:>FlagsAM[];
            processedRsys = processedRsys/.GtMacro[rxns_]:>IfGT[rxns];
            processedRsys = processedRsys/.GeMacro[rxns_]:>IfGE[rxns];
            processedRsys = processedRsys/.EqMacro[rxns_]:>IfEQ[rxns];
            processedRsys = processedRsys/.LtMacro[rxns_]:>IfLT[rxns];
            processedRsys = processedRsys/.LeMacro[rxns_]:>IfLE[rxns];
            processedRsys = processedRsys/.IfPresentMacro[spcs_, rxns_]:>TriggerIfPresent[spcs, rxns];
            processedRsys = Flatten[Append[processedRsys,{InitializeFlagConcentrations[]}]];
            processedRsys = PhasifyRsys[processedRsys];
            Return[processedRsys];
        ]
    )
    
ExpandRsys[rsys_] :=
    (
        Module[ {processedRsys},
            processedRsys = rsys;
            processedRsys = StepToPhase[processedRsys];
            processedRsys = processedRsys/.LdMacro[a_,b_]:>Ld[a,b];
            processedRsys = processedRsys/.AddMacro[a_,b_,c_]:>Add[a,b,c];
            processedRsys = processedRsys/.SubMacro[a_,b_,c_]:>Sub[a,b,c];
            processedRsys = processedRsys/.MulMacro[a_,b_,c_]:>Mul[a,b,c];
            processedRsys = processedRsys/.DvdMacro[a_,b_,c_]:>Dvd[a,b,c];
            processedRsys = processedRsys/.SqrMacro[a_,b_]:>Sqr[a,b];
            processedRsys = processedRsys/.AMMacro[x_, y_]:>AM[x, y];
            processedRsys = processedRsys/.NormalizeToFlagsMacro[x_, y_]:>NormalizeToFlags[x, y];
            processedRsys = processedRsys/.FlagsAMMacro[]:>FlagsAM[];
            processedRsys = processedRsys/.GtMacro[rxns_]:>IfGT[rxns];
            processedRsys = processedRsys/.GeMacro[rxns_]:>IfGE[rxns];
            processedRsys = processedRsys/.EqMacro[rxns_]:>IfEQ[rxns];
            processedRsys = processedRsys/.LtMacro[rxns_]:>IfLT[rxns];
            processedRsys = processedRsys/.LeMacro[rxns_]:>IfLE[rxns];
            processedRsys = processedRsys/.IfPresentMacro[spcs_, rxns_]:>TriggerIfPresent[spcs, rxns];
            processedRsys = Flatten[Append[processedRsys,{InitializeFlagConcentrations[]}]];
            processedRsys = PhasifyRsys[processedRsys];
            Return[processedRsys];
        ]
    )
    
StepToPhase[rsys_] :=
(
  Module[ {processedRsys, currentElem, i, reactions, operandA, operandB, firstPhaseReactions},
  processedRsys = rsys;
	For[i = 1, i <= Length[processedRsys], i++,
      currentElem = processedRsys[[i]];
	  If[MatchQ[currentElem, StepMacro[_]],
                reactions = currentElem /. StepMacro[rxns_] -> rxns;
                If[Length[Cases[reactions, CmpMacro[_,_]]] > 0,
                  operandA = Cases[reactions, CmpMacro[a_,b_]->a];
                  operandB = Cases[reactions, CmpMacro[a_,b_]->b];
                  operandA = operandA[[1]];
                  operandB = operandB[[1]];
                  firstPhaseReactions = DeleteCases[reactions, CmpMacro[_,_]];
                  firstPhaseReactions = Append[firstPhaseReactions, NormalizeToFlagsMacro[operandA, operandB]];
                  processedRsys = Delete[processedRsys, i];
                  processedRsys = Insert[processedRsys, Global`phase[firstPhaseReactions], i];
                  processedRsys = Insert[processedRsys, Global`phase[{FlagsAMMacro[]}], i + 1]; 
                ];
              ]
            ];
            processedRsys = processedRsys/.StepMacro[rxns_]:>Global`phase[rxns];
   Return[processedRsys];
   ]
)

(**
COMPOSIBILITY for Ld, Add, Mul, Div, Sub
From paper:
For given inputs a and b2, . . . , bm, the combined network has a unique asymptotically stable fixed point (x\:02c6\[Eth]a\[CapitalThorn], y\:02c6\[Eth]x\:02c6l \[Eth]a\[CapitalThorn], b2, . . . , bm)), and the convergence to this fixed point is again exponential.
Because the second network does not change the concentration of xl, the first network is independent of the second. 
**)

(**
Loads a into b; a remains unchanged.
Initial value of b does not affect the final result.
**)
Ld[a_,b_] :=
    (
        Module[ {trash},
            Sequence@@Flatten[
            {
               rxn[a,a+b,1],
               rxn[b,trash,1]
            }]
        ]
    )
    
(**
Computes c=a*b; a,b remain unchanged.
Initial value of c does not affect the final result.
**)
Mul[a_,b_,c_] :=
    (
        Module[ {trash},
            Sequence@@Flatten[
            {
               rxn[a+b,a+b+c,1],
               rxn[c,trash,1]
            }]
        ]
    )
    
(**
Computes c=a/b; a,b remain unchanged.
Initial value of c does not affect the final result.
**)
Dvd[a_,b_,c_] :=
    (
        Sequence@@Flatten[
        {
            rxn[a,a+c,1],
            rxn[b+c,b,1]
        }]
    )
    
(**
Computes c=a+b; a,b remain unchanged.
Initial value of c does not affect the final result.
c has to be different from a and b; while a and b can be the same species
**)
Add[a_,b_,c_] :=
    (
        Module[ {trash},
            Sequence@@Flatten[
            {
                rxn[a,a+c,1],
                rxn[b,b+c,1],
                rxn[c,trash,1]
            }]
        ]
    )
    
(**
Computes c=a-b, if a > b; otherwise c=0. 
a,b remain unchanged.
Initial value of c does not affect the final result.
**)
Sub[a_,b_,c_] :=
    (
        Module[ {helper,trash},
            Sequence@@Flatten[
            {
                rxn[a,a+c,1],
                rxn[b,b+helper,1],
                rxn[c,trash,1],
                rxn[c+helper,trash,1]
            }]
        ]
    )
    
(**
Computes c=a-b, if a > b; otherwise c=0. 
a,b remain unchanged.
Initial value of c does not affect the final result.
**)
Sqr[a_,b_] :=
  (
    Sequence@@Flatten[
    {
      rxn[a,a+b,2],
      rxn[b+b,1,1]
    }]
  )
    
(* Approximate Majority from Cardelli's paper *)
AM[x_, y_] :=
    (
        Module[ {b},
            Sequence @@ Flatten[
              {
                   rxn[x + y, y + b, 1],
                   rxn[b + y, y + y, 1],
                   rxn[y + x, x + b, 1],
                   rxn[b + x, x + x, 1]
               }]
        ]
      )

InitializeFlagConcentrations[] :=
    (
        Sequence@@Flatten[
        {
            conc[XgtyFlag, 0.5],
            conc[XltyFlag, 0.5],
            conc[YgtxFlag, 0.5],
            conc[YltxFlag, 0.5],
            conc[ComparisonOffset, 0.5]
        }]
    )

NormalizeToFlags[x_, y_] :=
    (
        Sequence @@ Flatten[
        {
            rxn[XgtyFlag + y, XltyFlag + y, 1],
            rxn[XltyFlag + ComparisonOffset, XgtyFlag + ComparisonOffset, 1],
            rxn[XltyFlag + x, XgtyFlag + x, 1],
            
            rxn[YgtxFlag + x, YltxFlag + x, 1],
            rxn[YltxFlag + ComparisonOffset, YgtxFlag + ComparisonOffset, 1],
            rxn[YltxFlag + y, YgtxFlag + y, 1]
        }]
    )

FlagsAM[] :=
    (
        Sequence @@ Flatten[
        {
            AM[XgtyFlag, XltyFlag],
            AM[YgtxFlag, YltxFlag]
        }]
    )

IfGT[rxns_] :=
    (
        Sequence @@ Flatten
        [
            rxns/.rxn[rs_,ps_,k_]->rxn[Evaluate[XgtyFlag+YltxFlag+rs],Evaluate[XgtyFlag+YltxFlag+ps],k]
        ]
    )

IfLT[rxns_] :=
    (
        Sequence @@ Flatten
        [
            rxns/.rxn[rs_,ps_,k_]->rxn[Evaluate[YgtxFlag+XltyFlag+rs],Evaluate[YgtxFlag+XltyFlag+ps],k]
        ]
    )

IfEQ[rxns_] :=
    (
        Sequence @@ Flatten
        [
            rxns/.rxn[rs_,ps_,k_]->rxn[Evaluate[XgtyFlag+YgtxFlag+rs],Evaluate[XgtyFlag+YgtxFlag+ps],k]
        ]
    )

IfGE[rxns_] :=
    (
        Sequence @@ Flatten
        [
            rxns/.rxn[rs_,ps_,k_]->rxn[Evaluate[XgtyFlag+rs],Evaluate[XgtyFlag+ps],k]
        ]
    )

IfLE[rxns_] :=
    (
        Sequence @@ Flatten
        [
            rxns/.rxn[rs_,ps_,k_]->rxn[Evaluate[YgtxFlag+rs],Evaluate[YgtxFlag+ps],k]
        ]
    )

GetTimeOfPhaseMaxima[tickId_] :=
(
  Module[ {
  	firstTick,
  	tickInterval,
  	tickPickTime},
      firstTick = 18.135824374899833;
      tickInterval = 36.098357644;
      tickPickTime = firstTick+tickId*tickInterval;
      Return[tickPickTime];
   ]
)
    
EvaluateError[rsys_,tmax_] :=
    (
      Module[ {
        numberOfPhases,
        expandedRsys,
        allSpecies,
        userDefinedSpecies,
        name,
        value,
        tickId,
        tickPickTime,
        phaseId,
        currentPhaseRxns,
        rxnsPerPhase,
        spcs,
        simulatedValue,
        map,
        sol,
        elem,
        val,
        gt,
        ge,
        eq,
        lt,
        le,
        ind},
          expandedRsys = ExpandRsys[rsys];
          sol = SimulateRxnsys[expandedRsys,tmax];
          allSpecies = SpeciesInRxnsys[expandedRsys];
          rxnsPerPhase = Cases[StepToPhase[rsys], Global`phase[rxns_] -> rxns];
          numberOfPhases = Length[rxnsPerPhase];
 
          userDefinedSpecies = Select[allSpecies,!StringContainsQ[ToString[#],"`"] &];
          map = Association[{}];
          For[i = 1,i<=Length[userDefinedSpecies],i++,
            (* assign default value - 0 *)
            name = userDefinedSpecies[[i]];
            val[name] = 0;
            AssociateTo[map,{name->{}}];
          ];
          For[i = 1,i<=Length[rsys],i++,
            elem = rsys[[i]];
            If[ MatchQ[elem,conc[_,_]],
                name = elem/.conc[a_,b_]->a;
                value = elem/.conc[a_,b_]->b;
                val[name] = value;
            ]
          ];
          tickId = 0;
          (* signals *)
          gt=False;
          ge=False;
          eq=False;
          lt=False;
          le=False;
          
          EvaluateCorrect[reactions_] :=
          (
            For[ind=1,ind<=Length[reactions],ind++,
              (* Loop needed to ensure all modules converge *)
              (* more optimized version is to loop until no value is affected by updates *)
	          Cases[reactions,LdMacro[a_,b_]:>Set[val[b] , val[a]]];
              Cases[reactions,AddMacro[a_,b_,c_]:>Set[val[c] , val[a]+val[b]]];
              Cases[reactions,SubMacro[a_,b_,c_]:>Set[val[c] , Max[0,val[a]-val[b]]]];
              Cases[reactions,MulMacro[a_,b_,c_]:>Set[val[c] , val[a]*val[b]]];
              Cases[reactions,DvdMacro[a_,b_,c_]:>Set[val[c] , val[a]/val[b]]];
              Cases[reactions,SqrMacro[a_,b_]:>Set[val[b] , Sqrt[val[a]]]];
            ];
            If[gt,Cases[reactions,GtMacro[rxns_]:>EvaluateCorrect[rxns]]];
            If[ge,Cases[reactions,GeMacro[rxns_]:>EvaluateCorrect[rxns]]];
            If[eq,Cases[reactions,EqMacro[rxns_]:>EvaluateCorrect[rxns]]];
            If[lt,Cases[reactions,LtMacro[rxns_]:>EvaluateCorrect[rxns]]];
            If[le,Cases[reactions,LeMacro[rxns_]:>EvaluateCorrect[rxns]]];
            Cases[reactions,NormalizeToFlagsMacro[a_,b_]:>
              (
                If[val[a]>val[b],
                  Set[gt,True]; Set[ge,True];Set[eq,False];Set[le,False];Set[lt,False];
                ];
                If[val[a]==val[b],
	              Set[gt,False]; Set[ge,True];Set[eq,True];Set[le,True];Set[lt,False];
                ];
                If[val[a]<val[b],
                  Set[gt,False]; Set[ge,False];Set[eq,False];Set[le,True];Set[lt,True];
                ];
              )
            ];
          );
          
          While[True,
            tickPickTime = GetTimeOfPhaseMaxima[tickId];
            If[ tickPickTime>tmax,
                Break[]
            ];
            phaseId = Mod[tickId,numberOfPhases]+1;
            currentPhaseRxns = rxnsPerPhase[[phaseId]];
            EvaluateCorrect[currentPhaseRxns];
            
            For[i = 1,i<=Length[userDefinedSpecies],i++,
              spcs = userDefinedSpecies[[i]];
              simulatedValue = EvaluateRxnAtPoint[sol,spcs,tickPickTime];
              AppendTo[map[spcs],{<|"tickId"->tickId,"time"->tickPickTime,"phaseId"->phaseId,"correctValue"->val[spcs],"simulationValue"->simulatedValue,"error"->Abs[val[spcs]-simulatedValue]|>}];
            ];
            tickId++;
          ];
          Return[map];
      ];
);

End[];

EndPackage[];