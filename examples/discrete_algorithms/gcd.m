Get[NotebookDirectory[]<>"init.m"];

GCDRsys[a0_, b0_] :=
  (
    crn = {
      conc[a,a0], 
      conc[b,b0],
      step[{
        ld[a, atmp],
        ld[b, btmp], 
        cmp[a,b]
      }],
      step[{
        ifGT[{ sub[atmp,btmp,a] }],
        ifLT[{ sub[btmp,atmp,b] }]
      }]
    };
    Return[crn];
  )
    
GCDRsysErrorCorrecting[a0_, b0_] :=
  (
    crn = {
        conc[a,a0],
        conc[b,b0],
        step[{
            ld[a, atmp],
            ld[b, btmp]
        }],
        step[{
            cmp[a,b]
        }],
        step[{
            ifGT[{
                sub[atmp,btmp,a]
            }],
            ifLT[{
                sub[btmp,atmp,b]
            }],
            ifEQ[{
                ld[atmp,a],
                ld[btmp,b]
            }]
        }]
    };
    Return[crn];
  )
    
GCDRsysWithOutput[a0_, b0_] :=
  (
    crn = {
        conc[a,a0],
        conc[b,b0],
        conc[outTmp, 1],
        conc[outFlag, 0],        
        step[{
            ld[a, atmp],
            ld[b, btmp],
            ld[outTmp, outFlag]
        }],
        step[{
            cmp[a,b]
        }],
        step[{
            ifGT[{
                sub[atmp,btmp,a]
            }],
            ifEQ[{
                ifPresent[outFlag, {ld[atmp,out]}],
                (* can also use destructive load r0tmp -> out *)
                rxn[outTmp, 0, 1]
            }],
            ifLT[{
                sub[btmp,atmp,b]
            }]
        }]
    };
    Return[crn];
  )
