Get[NotebookDirectory[]<>"init.m"];

(*
 Integer square root
   {{ X=m }}
   Z ::= 0;;
   WHILE (Z+1)*(Z+1) <= X DO
     Z ::= Z+1
   END
     {{ Z*Z<=m /\ m<(Z+1)*(Z+1) }}
*)
IntSqrtRsys[n0_] :=
  (
    crn = {
      conc[one,1], conc[n,10],
      step[{
        add[z,one,znext],
        mul[znext,znext,zpow],
        cmp[zpow,n]
      }],
      step[{
        ifLT[{ld[znext,z]}],
        ifGE[{ld[z,out]}]
      }]
    };
    Return[crn];
  )
 
