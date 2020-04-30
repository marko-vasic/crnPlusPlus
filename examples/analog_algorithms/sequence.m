Get[NotebookDirectory[]<>"init.m"];

SequenceRsys[] :=
(
  crn = {
    conc[a, 3],
    step[{
      rxn[a, b, 1]
    }],
    step[{
      rxn[b, c, 1]
    }],
    step[{
      rxn[c, a, 1]
    }]
  };
  Return[crn];
)
