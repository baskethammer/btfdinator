with qprices as
     (select symbol as psyms, fdate from prices
      where close > 10 and close <50 and avgvol_50d > 200000),
     qstats as
      (select symbol as ssyms from sharesstats
        where sharesfloat > 5000000 and sharesfloat <> ''
        and percentinstitutions > 5 and percentinstitutions <> ''
	and percentinsiders > 25 and percentinsiders <> ''),
     qearns as
      (select symbol as esyms from earnings_history where
        q1>0 and q1 <> ''),
     dates as
      (select max(fdate) as mdate from prices)
select p.psyms from qprices p, qstats s, qearns e, dates d
 where p.psyms = s.ssyms and
       p.psyms = e.esyms and
       p.fdate = d.mdate
