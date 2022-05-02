## Resubmission

This is a resubmission. In this version I have:

+ Fixed an error which was caused by `ir::ir_import_spc()` on r-devel.

Apart from minor changes (e.g. using smaller data in examples), there are no additional changes.

---------------------------------------

I am sorry for this soon resubmission, but shortly after the accepted CRAN submission, I received a notification that 'ir' produces an error on 'r-devel' and that I have to resubmit the package to avoid removal from CRAN in about a week.

The error is caused by `hyperSpec::read.spc()` which is used in `ir::ir_import_spc()`. I have contacted the maintainers of 'hyperSpec' and they resolved this in the GitHub repository (See https://github.com/r-hyperspec/hyperSpec/issues/80), but I am afraid there is not enough time for them to fix this on CRAN.

For this reason, I decided to allow the user to specify the argument which causes the error in `hyperSpec::read.spc()`. This allows to circumvent the error at the cost of not importing all metadata. This has no influence on other functionalities of the 'ir' package. 

I added a note that this error will be fixed soon and that it can currently be avoided by using the development version of 'hyperSpec'.


## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## revdepcheck results

I checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * I saw 0 new problems
 * I failed to check 0 packages
