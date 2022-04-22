## Resubmission

This is a resubmission.

### Issue 1:

> Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar)  
> Missing Rd-tags:  
>      ir_bc_sg.Rd: \value  
>      Ops.ir.Rd: \value 

I've added `\value` tags to `ir_bc_sg.Rd` and `Ops.ir.Rd`as requested.

### Issue 2:

> You have examples for unexported functions.  
   distinct.ir() in:  
      group_by.Rd  
      ir_as_ir.Rd  
      summarize.Rd  
   ir_normalize() in:  
      ir_as_ir.Rd  
      mutate.Rd  
      rowwise.ir.Rd  
   ir_normalise() in:  
      nest.Rd  
   ir_to_absorbance() in:  
      rename.Rd  
   left_join.ir() in:  
      ir_bc.Rd  
      slice.Rd  
   full_join.ir() in:  
      summarize.Rd  
 Please either omit these examples or export the functions.   

I think I do not fully understand the problem here:

* For `distinct.ir()`:  
   1. In `group_by.Rd`, I cannot find `distinct.ir()` being used in an example.
   2. In `ir_as_ir.Rd`, I cannot find `distinct.ir()` being used in an example.
   3. In `summarize.Rd`, I cannot find `distinct.ir()` being used in an example.
   
* For `ir_normalize()`: I think this function is exported (there is a line `export(ir_normalize)` in the `NAMESPACE`). I also don't see it being used in `ir_as_ir.Rd`, `mutate.Rd`, and `rowwise.ir.Rd`.

* For `ir_normalise()`: I have the same observation as for `ir_normalize()` (I think the function is exported). In addition, I also don't see `ir_normalise()` being used in `nest.Rd`.

* For `ir_to_absorbance()`: I have the same observation as for `ir_normalize()` (I think the function is exported). In addition, I also don't see `ir_to_absorbance()` being used in `rename.Rd`.

* For `left_join.ir()`: I have the same observation as for `distinct.ir()`: I don't see the function being used in `ir_bc.Rd` or `slice.Rd`.

* For `full_join.ir()`: I have the same observation as for `distinct.ir()`: I don't see the function being used in `summarize.Rd`.

Perhaps I just don't correctly understand the issue since I am a beginner. In this case, could you please explain the problem in more detail? 

Thank you!

Henning Teickner


## Submission 1

### R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

> New submission

> Possibly misspelled words in DESCRIPTION:
   Galactic's (12:10)
   Preprocess (2:32)
   Thermo (12:3)

These are either proper names (Thermo Galactic is a company) or alternative formulations (preprocess; see: https://en.wiktionary.org/wiki/preprocess).

> Found the following (possibly) invalid URLs:
   URL: https://doi.org/10.1002/0470011149
     From: man/ir_to_transmittance.Rd
     Status: 503
     Message: Service Unavailable

I think this is a false positive. On my machine and web browser, I can access the referenced site.
