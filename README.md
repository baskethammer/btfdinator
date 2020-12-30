# BTFDINATOR

This is a stock screen to run against [YEODL](https://github.com/baskethammer/yeodl).  
## License

GNU General Public License V3

## Description

The Btfdinator screen is based on the "[Jack Hershey Equities System](https://www.elitetrader.com/et/threads/spydertraders-jack-hershey-equities-journal.38777/page-206#post-859367)". It looks for equities meeting certain basic criteria (included in the screen2.sql file here) that have had five or more 20% runs in the last six months.

## Getting Started

Clone or download this to a directory where [quicklisp](https://www.quicklisp.org/beta/) can find it.

```lisp
CL-USER> (ql:quickload :btfdinator)
To load "btfdinator":
  Load 1 ASDF system:
    btfdinator
; Loading "btfdinator"
..

(:BTFDINATOR)
CL-USER> (in-package btfdinator)
#<PACKAGE "BTFDINATOR">
BTFDINATOR> (run-btfd-list "/home/bh/screen2.sql" "/home/bh/yeodl/dbs/US.db")
(("VOXX" 39694.164 237500 12.94) ("JWN" 2529763.5 15136200 30.32)
 ("BKE" 91984.17 550364 29.85) ("CONN" 77182.17 461800 12.83))
BTFDINATOR> 
```
Run-btfd-list will output stocks meeting the scan criteria, their "dry up" thresholds (i.e. volume required by 11.30 am), and their "first rising volume" thresholds, as well as yesterday's close for reference.

### Dependencies

Depends on [YEODL](https://github.com/baskethammer/yeodl), [YEODL-cl](https://github.com/baskethammer/yeodl-cl), and a Common Lisp implementation (this was developed with [SBCL](https://www.sbcl.org))

## Authors


[Basket Hammer](bh@baskethammer.com)
[@baskethammer](https://twitter.com/baskethammer)

## Version History

    * Initial Release

## License

This project is licensed under the GNU General Public License (v3) - see the LICENSE.md file for details

