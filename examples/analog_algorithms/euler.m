Get[NotebookDirectory[]<>"init.m"];

EulerRsys[] :=
(
  crn = {
    conc[e, 1], conc[element, 1],
    conc[divisor, 1], conc[divisorMultiplier, 1],
    conc[one, 1],
    step[{
      div[element, divisor, elementNext],
      add[divisor, one, divisorNext],
      add[e, elementNext, eNext]
    }],
    step[{
      ld[elementNext, element],
      ld[divisorNext, divisor],
      ld[eNext, e]
    }]
  };
  Return[crn];
)
