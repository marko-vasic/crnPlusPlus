Get[NotebookDirectory[]<>"init.m"];

FibonacciRsys[] :=
(
  crn = {
    conc[f0,0],
    conc[f1,1],
    step[{
      ld[f0,f0tmp],
      ld[f1,f1tmp]
    }],
    step[{
      add[f0tmp,f1tmp,f1],
      ld[f1tmp,f0]
    }]
  };
  Return[crn];
);
