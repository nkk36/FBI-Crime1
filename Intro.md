# U.S. Crime  
This is a web application for Table 1 of FBI Uniform Crime Reporting (UCR) data using Shiny in R.

The data is based on Table 1 of the [2015 FBI UCR](https://ucr.fbi.gov/crime-in-the-u.s/2015/crime-in-the-u.s.-2015/offenses-known-to-law-enforcement/violent-crime)

This web app contains tabs that display crime data based on Table 1 of the 2015 FBI UCR and allow the user to change various parameters to view see how crime has changed over time. You can see the code behind the app by visting my [GitHub](https://github.com/nick898)

The columns or fields of the table are as follows:

1. Year
2. Population
3. Number of Violent Crimes
4. Violent Crime Rate
5. Number of Murders
6. Murder Rate
7. Number of Rapes (Revised definition)
8. Rape Rate (Revised definition)
9. Number of Rapes (Legacy definition)
10. Rape Rate (Legacy definition)
11. Number of Robberies
12. Robbery Rate
13. Number of Aggravated Assaults
14. Aggravated Assault Rate
15. Number of Property Crimes
16. Property Crime Rate
17. Number of Burglaries
18. Burglary Rate
19. Number of Larceny-Thefts
20. Larceny-Theft Rate
21. Number of Motor Vehicle Thefts
22. Motor Vehicle Theft Rate

**All rates are calculated so that they represent the number of crimes per 100,000 people.**

# Definitions and Caveats

**Violent Crime:**

Violent crime includes murder, rape, robbery, and aggravated assault. The violent crime numbers (count and rate) do not include the revised definition of rape. For Table 1 of the UCR data the FBI defines total violent crime as being the total number of murders, rape (legacy definition), robberies, and aggravated assaults in a given year. Rapes that meet the revised definition did not begin being collected until 2013 so the data on rapes defined by the revised definition are unknown prior to 2013.

**Property Crime:**

Property crime includes burglaries, larceny-thefts, and motor-vehicle thefts.
