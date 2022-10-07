## Resubmission
This is a resubmission. In this version I have:

* Removed all uses of '<<-'

The response to the original submission stated:
"If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file".

The package does not reference academic publications.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

There is also one NOTE that is only found on Windows Server 2022, R-devel, 64 bit:
```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```

As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.
