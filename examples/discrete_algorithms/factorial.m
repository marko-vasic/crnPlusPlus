Get[NotebookDirectory[]<>"init.m"];

Fact[f0_] :=
(
  crn={
    conc[f,1], conc[one,1], conc[i,f0],
    step[{
      cmp[i,one],
      mul[f,i,fnext],
      sub[i,one,inext]
    }],
    step[{
      ifGT[{    
        ld[inext,i],
        ld[fnext,f]
      }]
    }]
  };
  Return[crn];
);
