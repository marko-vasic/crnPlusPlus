Get[NotebookDirectory[]<>"init.m"];

(*
Division(A,B)
  while A >= B
  begin
    A := A - B
    Q := Q + 1
    R := A
  end
*)
DiscreteDivisionRsys[a0_, b0_] :=
(
  crn = {
    conc[a,a0], conc[b,b0],
    conc[q,0], conc[r,0],
    conc[one,1],
    step[{
      cmp[a,b]
    }],
	step[{
	  ifGE[{
	    sub[a,b,anext],
	    add[q,one,qnext]
	  }]
	}],
	step[{
	  ifGE[{
	    ld[anext,a],
	    ld[qnext,q]
	  }],
	  ifLT[{
	    ld[a,r]
	  }]
	}]
  };
  Return[crn];
)
