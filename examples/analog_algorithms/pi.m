Get[NotebookDirectory[]<>"init.m"];

PiRsys[] :=
(
  crn = {
    conc[four, 4],
    conc[divisor1, 1],
    conc[divisor2, 3],
    conc[pi, 0],
    step[{
      div[four, divisor1, factor1],
      add[divisor1, four, divisor1Next],
      div[four, divisor2, factor2],
      add[divisor2, four, divisor2Next],
      sub[factor1, factor2, factor],
      add[pi, factor, piNext]
    }],
    step[{
      ld[divisor1Next, divisor1],
      ld[divisor2Next, divisor2],
      ld[piNext, pi]
    }]
  };
  Return[crn];
)
