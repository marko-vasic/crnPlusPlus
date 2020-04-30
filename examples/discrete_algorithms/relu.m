Get[NotebookDirectory[]<>"init.m"];

ReLURsys[xp0_,xm0_] :=
(
  crn = {
    conc[xp,xp0],
    conc[xm,xm0],
    conc[yp,0],
    conc[ym,0],
    step[{
      cmp[xp,xm]
    }],
    step[{
      ifGE[{
        ld[xp,yp],
        ld[xm,ym]
      }]
    }]
  };
  Return[crn];
);
