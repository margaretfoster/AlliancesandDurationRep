"Terrorist Group Cooperation and Longevity," International Studies Quarterly, Brian J. Phillips*Table 1*Model 1, baseline modellogit failure size religious ethnic drugs statesponsored wbgi_gee_cs polity2 logpop namerica latamerica ssafrica meast asia spline1 spline2 spline3, robust cluster(groupid) estat ic*Model 2, allieslogit failure allies size religious ethnic drugs statesponsored wbgi_gee_cs polity2 logpop namerica latamerica ssafrica meast asia spline1 spline2 spline3, robust cluster(groupid) estat ic*Model 3, allies + eigenvector centralitylogit failure allies alliesties size religious ethnic drugs statesponsored wbgi_gee_cs polity2 logpop  namerica latamerica ssafrica meast asia spline1 spline2 spline3, robust cluster(groupid)estat ic*Model 4 interaction termslogit failure allies alliesXgee alliesXpolity2 size religious ethnic drugs statesponsored wbgi_gee_cs polity2 logpop namerica latamerica ssafrica meast asia spline1 spline2 spline3, robust cluster(groupid) estat ic*Graphing (requires Stata 12)logit failure  c.allies##c.wbgi_gee_cs c.allies##c.polity2  size religious ethnic drugs statesponsored wbgi_gee_cs polity2 logpop namerica latamerica ssafrica meast asia spline1 spline2 spline3, robust cluster(groupid) margins, dydx(allies) at(wbgi_gee_cs=(-2(.5)2)) vsquishmarginsplot, nocimargins, dydx(allies) at(polity2=(-10(2)10)) vsquishmarginsplot, noci