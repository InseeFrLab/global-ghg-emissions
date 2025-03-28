# Building global Air Emission Accounts (AEAs) compatible with FIGARO

The aim of this project is to estimate global "Air Emission Accounts" (=
GHG emissions per country X industry), at a level of detail compatible
with the FIGARO multi-regional input-output tables produced by the
European Commission. The combination of those global AEAs with the
FIGARO tables enables "environmentally extended" input-output analysis,
such as carbon footprint calculation.

The first phase of the project aims to replicate the methodology
designed by Eurostat to estimate global AEAs starting from the EDGAR
database. The second phase will aim to build on this work to provide new
details on the GHG emissions.

## 1. Replication of Eurostat's methodology to estimate global AEAs based on EDGAR

### 1.1. How it works

Eurostat has developed a methodology to build global AEAs compatible
with FIGARO, starting from the EDGAR database. The method is explained
in the following paper :
<https://ec.europa.eu/eurostat/documents/1798247/6191529/Methodological+Note_GHG_estimates_FIGARO_21_June_2024/b23da1a7-d8bb-6834-0608-7fc9bb293ddc?t=1719236345901>
(the PDF included in the GIT repo for reference, in case the link above
is broken)

The inputs needed are all available online :

-   The EDGAR database

-   The FIGARO database

-   Some PEFA results for EU (used only to compute a households /
    industry share of road transport emissions)

-   The OECD database on aviation emissions

-   The OECD database on maritime emissions

-   The UK air emission accounts disseminated by the ONS

The GIT repo does not include these source files, but includes text
instructions on how to download the sources.

Regarding the use of the EDGAR database, please note that for non-European countries,
EDGAR uses data from IEA Greenhouse Gas Emissions from Energy (http://www.iea.org/data-and-statistics),
as modified by the Joint Research Centre, licensed under CC BY-NC-ND 4.0.
Users of the EDGAR data for those countries should contact the IEA at compliance@iea.org
if they wish to use them outside the terms of the CC-BY-NC-ND 4.0 licence,
in particular if they wish to use them for commercial purposes.

After the user has downloaded the sources and placed them in the
suitable directories, the scripts go as follows :

-   pre-processing of the sources (1 script per source) : to extract
    only the relevant information from the source and format it in a
    consistent way

-   estimation of the global AEAs : this is the core of the program. The
    logic is based on Eurostat's paper.

-   test of the output validity : to check that everything went well

-   *[for information only]* comparison with Eurostat's own estimates :
    to assess the quality of the replication job

### 1.2. Differences with Eurostat's own estimates

We need to distinguish several groups of countries.

For Switzerland (CH), Turkey (TR) and Norway (NO), the current program
(as of January 2025) does not fully replicate Eurostat's paper. The AEAs
for those countries are fully estimated based on EDGAR, but the
estimation is not put back in line with the partial AEAs that those
countries report to Eurostat. This will be amended in a future version
of the program.

For EU27 countries, the AEAs are directly taken from Eurostat's website.
Here, the only difference with Eurostat's own results are due to vintage
differences. We must note in particular that the last year available on
Eurostat's website (year T-2) is often the result of a forecast done by
Eurostat, rather than a figure reported by the country. This implies
that revisions for this year can be significant when the country
eventually reports a figure.

The same applies for the UK : a difference in vintage can exist between
this program results and Eurostat's estimates, depending on when the
dataset was downloaded from the ONS website.

Finally, all other countries are fully estimated based on the EDGAR
database. In theory, there should be no difference between the results
of this program and Eurostat's estimates. However, for the purpose of
CRF -\> NACE allocation, Eurostat does use an internal version of the
FIGARO tables, rather than the publicly available version. The two
versions may differ slightly for confidentiality reasons. As a
consequence, the resulting emissions by NACE may also slightly differ
between the two estimates.

### 1.3. Comments on the methodology

We can draw several lessons from the replication of Eurostat's
methodology :

-   Services (NACE I to U) receive most of their emissions from
    only two IPCC codes ("Residential and other sectors" and "Road
    transportation"). At the begining of the procedure, these two IPCC codes
    are split between households direct emissions and industries
    emissions, using allocation keys which are not always country specific.
    We can therefore conclude that the general level of GHG
    emissions in NACE services, as well as the distribution between
    services, is somewhat fragile.

-   Eurostat's approach of using a fixed distribution key over the whole
    time-series to allocate emissions from CRF to NACE is fairly
    conservative. In theory, the distributions keys could be
    re-estimated for each year, although this would of course result in
    more volatile AEAs.

## 2. Extensions

### 2.1. Global AEAs based on PRIMAP

To explore the sensitivity of the French footprint to the source choice for global GHG emissions,
we estimated two variants of the global AEAs, based on alternative sources :
PRIMAP-CR (for "country reported") and PRIMAP-TP ( for "third party").

Those sources are available in the CRF nomenclature, on a territorial emissions basis.
In order to build complete AEAs from PRIMAP, we proceed in two steps :
first, we expand the level of detail of some CRF categories in PRIMAP,
based on the more detailed structure found in EDGAR; then we apply the Eurostat's methodology
described in the previous section to convert from the CRF nomenclature to NACE.

To account for international sea and air transport, we use the same sources
as in the Eurostat's methodology, that is the OECD dedicated databases on CO2 emissions from Air and Sea transport.

## 3. Possible extensions (to be done)

### 3.1. New details of CO2 emissions

Starting from the previous results, the goal would be to break the CO2
emission vector down in several categories :

-   CO2 from energy combustion, except road transportation

-   CO2 from road transportation

-   CO2 from cement production (chemical process)

-   CO2 from steel production (chemical process)

-   CO2 other

This extension would be relatively straightforward, as these categories
are distinguished in the IPCC 2006 nomenclature used in the EDGAR
database.

CO2 from energy combustion could be further broken down by fuel type
(eg. coal, oil products and natural gas), using the IEA world energy
balances.

### 3.2. New details by industry

Starting from the previous results, the goal would be to break down the
emission vectors with more industries, particularly in :

-   agriculture

-   mining and quarrying

Contrary to the previous one, this extension would require the use of
additional information, not currently found in EDGAR or FIGARO.
