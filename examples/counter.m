CounterRsys[c0_] :=
(
  crn = {
    conc[c,c0], conc[cInitial,c0],
    conc[one,1], conc[zero,0],
    step[{
      Sub[c,one,cnext],
      Cmp[c,zero]
    }],
    step[{
      IfGT[{Ld[cnext,c]}],
      IfLE[{Ld[cInitial,c]}]
    }]
  };
  Return[crn];
);
