# Western Atlantic Skipjack tuna Management Strategy Evaluation

## International Commission for the Conservation of Atlantic Tuna - ICCAT

# 1. Background

During 2014 meetings, the Standing Committee on Research and Statistics (SCRS) from the International Commission for the Conservation of Atlantic Tunas (ICCAT) has adopted the new science and strategic plan for his orientation and guidance over 2015-2020. In general, this plan comprises, among other aspects, goals, objectives, the strategies to achieve each objective, and measures targets. The plan was not static, and revisions according to requests made by the Commission could be provided. In this way, annual work plans for each of its' Sub-Committees and Species Groups to provide the Commission with the necessary advice to conserve and manage the different stocks can be provided.

An essential element of this plan was developing a robust advice framework consistent with the Precautionary Approach. This advice framework requires developing new stock assessment methods that consider the primary sources of uncertainty, incorporation of new data sets and knowledge provided by the SCRS Species Group. Another important addition to this advice was including methods to evaluate this novel approach based on the Management Strategy Evaluation (MSE) application. This process allows current and alternative assessment and advice frameworks to be evaluated concerning their ability to meet multiple management objectives with acceptable levels of risk.

The development of Management Strategy Evaluation for the Atlantic Tropical Tuna stocks started in 2018. This first work was based on an ICCAT contract to develop a concept of application MSE in distinct phases. The first phase of this concept, developed by a consortium of researchers (Merino et al., 2020), was to describe how to conduct the MSE process in a series of phases. Unfortunately, following the Commission indication to revise the schedules for the different ICCAT species MSE process, the development of the subsequent phases was not carried out until 2020, as described in Merino et al. (2020).

The MSE for Tropical Tunas has two components: a multi-stock MSE comprising bigeye tuna, yellowfin tuna and the eastern stock of skipjack and a single stock MSE for the western stock of skipjack. The multi-stock MSE is developing single operating models (OM) for each stock but will link these OMs through the modules that simulate fishery harvests and management procedures.

Reasons for developing an MSE for the western skipjack are multifold. Firstly, the stock is largely (90%) caught by a single Contracting Party and mainly by a single fishery (baitboats). Secondly, this fishery captures only small quantities of the other two tropical tuna species. Thirdly, the development of a single stock MSE should be much more straightforward than that of the multi-stock MSE. The combination of these three reasons will make the development and potential implementation of an MSE process an easier process. Stakeholders of tropical tunas will learn about the MSE process prior to fully engaging in the more complex process for the Multi-stock MSE. Developing the MSE for the western stock will also help increase the technical capacity and stakeholder engagement of MSE to support other stocks managed by ICCAT (Huynh et al., 2020).

During the Tropical Tuna Species Group meeting in 2020, a first demonstration of the MSE framework applied for western skipjack tuna was presented (Huynh et al., 2020 – SCRS/2020/140). The framework was reviewed by the Tropical Tuna MSE Technical Group in a meeting in March 2021. This preliminary MSE considered only the Southwestern Atlantic portion of the Western SKJ stock and included only harvests from the Brazilian bait boat and handline fleets. The model was built in the MSEtool R package using a Stock Reduction Analysis (SRA) approach for conditioning the Operating Models (OMs). In summary, a total of six OMs were explored, considering uncertainty in natural mortality, growth, maturity, selectivity, and steepness. A suite of example management procedures (MPs), including fixed TACs, index-slope MPs, and harvest control rules (HCRs), was tested in closed-loop simulation. The Tropical Tuna working group recommended some modifications to this initial framework, including the expansion of the framework to all the fishery harvesting the stock of western skipjack.

A revised MSE framework for western was developed during 2021 and 2022 (SCRS/2022/097, Mourato et al., 2022a) with the catch time series spanning from 1952 to 2020. Operating models were conditioned with catches, Catch Per Unit Effort (CPUE), and size data from 5 fleets: PS West, BB West, LL USMX, LL Others, and HL_RR. A set of OMs, covering uncertainty in life-history parameters were explored. The analysis also included initial trials of closed-loop simulations evaluating the relative performance of pre-selected management procedures across an initial set of performance metrics. The results of this update in W-SKJ MSE were presented in the 2022 Tropical Tuna MSE Technical Sub-group Meeting that occurs before the W-SKJ Stock Assessment. At such meeting the tropical tuna working group recommended that OMs for the MSE must be reconditioned following the 2022 skipjack stock assessment results and, if necessary, adjustments to the fleet structure must be considered.

After the Atlantic skipjack stock assessment (2022), the W-SKJ MSE operating models were reconditioned using the Stock Synthesis model results. The analysis also included evaluating the relative performance of pre-selected management procedures across a set of performance metrics. In general, simulations presented during the 2022 Species Group Meeting shown good performance metrics across management procedures regarding the safety, status, yield, and stability of Western Atlantic skipjack tuna (SCRS/2022/180, Mourato et al., 2022b). During the 27th Regular Meeting of the ICCAT Commission held in November 2022, the Commission adopted conceptual management objectives for W-SKJ (Res. 22-02). In 2023, these conceptual objectives were refined, now presenting levels of reference to be tested inside the MSE simulations for the four general management objective areas (e.g. Status, Safety, Yield and Stability). After the discussions held during the 28th Regular Meeting of the ICCAT Commission, the adoption of an MP for the W-SKJ stock was postponed for potential consideration in 2024. Throughout 2024, a series of new analyses were conducted, incorporating updated catch data and relative abundance indices provided by CPCs. During the year, new scenarios were simulated based on recommendations made by ICCAT commissioners. Following further discussions at the 24th Special Meeting of the ICCAT Commission, new Candidate Management Procedures were proposed to be evaluated by the SCRS during 2025, with potential consideration for the adoption of a management measure for the stock to be implemented in 2026.This will
permit further analysis and review by the SCRS, including climatic change robustness scenarios, as well as additional dialogue with Panel 1. The following tasks were developed taking into account the discussions and highlights provided by the ICCAT Commission and prior discussions during SCRS meetings.

# 2. MSE Overview

The W-SKJ MSE is built using an open-source MSE software package called [openMSE](https://openmse.com). The package can input information from assessment models built with the Stock Synthesis framework (the 2022 W-SKJ stock assessment, in this case) to efficiently create – and then customize – an MSE framework for testing candidate management procedures (CMPs), including the approximately 100 CMPs that come preloaded in openMSE.

## 2.1. Indices of Abundance

The western skipjack stock occurs from the U.S. coast to the southern Brazilian coast. Data from 5 different indices (baitboat – Brazil recent and earlier period, Brazil handline, Venezuela purse seine, and U.S. longline) are used to condition the MSE. On average, Brazil takes approximately 90% of the total skipjack catch in the West Atlantic, with the bulk of remaining catches (7% on average) taken by Venezuela. The MSE’s historical period is from 1952 through to 2020, and projections cover the subsequent 30 years.

## 2.2. Operating Models

Each operating model (OM) in the MSE represents a plausible scenario/a potential truth for the dynamics of the stock and fishery. The W-SKJ MSE includes 9 main operating models (i.e., the “reference set or grid of OMs”) based on two major sources of uncertainty:

1. Recruitment/steepness: a measure of the adult biomass relative to the number of young they produce; reflects stock productivity (3 options);

2. Growth vector: reflects the alternative biological parameters of the population, including different combinations of growth rate, maximum size, and natural mortality (3 options).

The 9 OMs allow for all combinations of these options (3x3=9). These 9 OMs were completely derived from the last stock assessment of the W-SKJ conducted in 2022 (Report of the 2022 Skipjack Stock Assessment Meeting). Thus, reflecting the same decision taking during the last stock assessment, the product of the all 9 OMs scenarios were also considered to be equally plausible, so they were equally weighted in this MSE. This nine OMs together consisting in the reference operating models.

There are also two sets of “robustness” OMs already implemented. They were implemented to evaluate less likely but still possible scenarios, similar to more extreme “sensitivity runs” in a stock assessment. These additional scenarios included a new uncertainty axis containing two new levels. Both addressed possible TAC implementation/control problems (e.g. 10% and 20% implementation error). This inclusion had resulted in 18 new robustness OMs (9x2=18).

# 3. Conceptual Management Objectives

- **Status**: The stock should have a 60% or greater probability of occurring in the green quadrant of the Kobe matrix over the medium-term (4-10 years) using a 30-year projection period;

- **Safety**: There should be no greater than 10% probability of the stock falling below B<sub>LIM</sub> (0.4*B<sub>MSY</sub>) at any point during the 30-year projection period;

- **Yield**: Maximize overall catch levels;

- **Stability**: Any changes in TAC between management periods should be 25% or less.

# 4. Work Plan for 2025

Below is described, in a hierarchical manner, the proposed work plan for the evolution and progress of the
W-SKJ MSE during 2025.

1) Reevaluate possible uncertainties in stock assessments and closed-loop simulations, as a form to avoid possible biologically implausible scenarios and present the results of these analysis during the Bigeye Data Preparatory. [**21 April 2025**];

2) Test and evaluate the new objectives defined by the ICCAT Commission into the MSE framework and present the results of these analysis during the Bigeye Stock Assessment meeting. [**14 July 2025**];

3) Implement robustness tests scenarios that incorporate potential effects of climate change on the western Atlantic skipjack tuna stock and present the results of these analysis during the Tropical Tuna Species Group meeting. [**22 September 2025**].

# 5. Data Requirements

1) Western skipjack tuna capture data must be updated by 2023.

2) The same must be done for the abundance indices that will be used by management procedures, which must be updated by 2023.

# 6. Code Development

Soon... very soon!!!

# 7. References

- Huynh Q. C., Carruthers T., Mourato B., Sant'Ana R., Cardoso LG., Travassos P. and Hazin F. 2020. A demonstration of a MSE framework for western skipjack tuna, including operating model conditioning. Collect. Vol. Sci. Pap. ICCAT, 77(8): 121-144.

- Merino G., A. Urtizberea, D. García, J. Santiago, H. Murua, W. Harford, J. Walter Jr., D. Gaertner 2020. Final Report of the ICCAT short-term contract: modelling approaches - support to ICCAT Tropical tunas MSE process. Collect. Vol. Sci. Pap. ICCAT, 76(6): 997-1009.

- Mourato B.L., Cardoso L.G., Arocha F., Narvaez M., Sant’Ana, R. 2022a. Western atlantic skipjack tuna MSE:
updates to the operating models and initial evaluation of the relative performance of preliminary management procedures. Collect. Vol. Sci. Pap. ICCAT, 79(1): 384-418.

- Mourato B.L., Cardoso L.G., Sant’Ana, R. 2022b. Management strategy evaluation for the western Atlantic skipjack tuna with operation model conditioning based on the Stock Synthesis model. Collect. Vol. Sci. Pap. ICCAT, 79(1): 851-906.