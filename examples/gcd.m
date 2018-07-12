GCDRsys[a0_, b0_] :=
  (
    crn = {
      conc[a,a0], 
      conc[b,b0],
      step[{
        Ld[a, atmp],
        Ld[b, btmp], 
        Cmp[a,b] 
      }],
      step[{
        IfGT[{ Sub[atmp,btmp,a] }],
        IfLT[{ Sub[btmp,atmp,b] }]
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
            Ld[a, atmp],
            Ld[b, btmp]
        }],
        step[{
            Cmp[a,b]
        }],
        step[{
            IfGT[{
                Sub[atmp,btmp,a]
            }],
            IfLT[{
                Sub[btmp,atmp,b]
            }],
            IfEQ[{
                Ld[atmp,a],
                Ld[btmp,b]
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
            Ld[a, atmp],
            Ld[b, btmp],
            Ld[outTmp, outFlag]
        }],
        step[{
            Cmp[a,b]
        }],
        step[{
            IfGT[{
                Sub[atmp,btmp,a]
            }],
            IfEQ[{
                IfPresent[outFlag, {Ld[atmp,out]}],
                (* can also use destructive load r0tmp -> out *)
                rxn[outTmp, 0, 1]
            }],
            IfLT[{
                Sub[btmp,atmp,b]
            }]
        }]
    };
    Return[crn];
  )