Get[NotebookDirectory[]<>"init.m"];

CounterRsys[c0_] :=
(
  crn = {
    conc[c,c0], conc[cInitial,c0],
    conc[one,1], conc[zero,0],
    step[{
      sub[c,one,cnext],
      cmp[c,zero]
    }],
    step[{
      ifGT[{ld[cnext,c]}],
      ifLE[{ld[cInitial,c]}]
    }]
  };
  Return[crn];
);
