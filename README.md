# Bachelor Thesis: Arctic greening in Models and Observations
Author: Valentin Lanz

Supervisors: Fabrice Lacroix, Benjamin Stocker

Degree program: Geography

### Overview

This repository contains the files related to the Arctic greening project. The aim of the project is to: 
1)	Provide a broad overview of trends in Arctic greening using Leaf-Area-Index (LAI) observations from 1982-2021.
2)	Test the performance of different Dynamic Global Vegetation Models (DGVMs) in modelling Arctic greening trends
3)	Examine if model accuracy depends on the inclusion of nutrient cycling

I used R 4.5.3 for the calculations.

### Folder structure

R scripts used for plotting are stored in the /analysis folder. R functions used for calculations are in /R. The plots and figures can be found in /fig. Some test data is stored in /data, but the main datasets were too large for GitHub. For information on how to obtain them, see below.


### Data

For LAI observation data I used the AVHRR product from (Jeong et al. 2024). You can access it here: https://www.sciencedirect.com/science/article/pii/S0034425724003006
Model data was downloaded from Trendyv14 ( https://globalcarbonbudgetdata.org/).

### Literature

Berner, L. T., Massey, R., Jantz, P., Forbes, B. C., Macias-Fauria, M., Myers-Smith, I., Kumpula, T., Gauthier, G., Andreu-Hayles, L., Gaglioti, B. V., Burns, P., Zetterberg, P., D’Arrigo, R., & Goetz, S. J. (2020). Summer warming explains widespread but not uniform greening in the Arctic tundra biome. Nature Communications, 11(1), 4621. https://doi.org/10.1038/s41467-020-18479-5

Friedlingstein, P., O’Sullivan, M., Jones, M. W., Andrew, R. M., Hauck, J., Landschützer, P., Le Quéré, C., Li, H., Luijkx, I. T., Olsen, A., Peters, G. P., Peters, W., Pongratz, J., Schwingshackl, C., Sitch, S., Canadell, J. G., Ciais, P., Jackson, R. B., Alin, S. R., … Zeng, J. (2025). Global Carbon Budget 2024. Earth System Science Data, 17(3), 965–1039. https://doi.org/10.5194/essd-17-965-2025

Frost, G. V., Bhatt, U. S., Macander, M. J., Berner, L. T., Walker, D. A., Raynolds, M. K., Magnússon, R. Í., Bartsch, A., Bjerke, J. W., Epstein, H. E., Forbes, B. C., Goetz, S. J., Hoy, E. E., Karlsen, S. R., Kumpula, T., Lantz, T. C., Lara, M. J., López-Blanco, E., Montesano, P. M., … Waigl, C. F. (2025). The changing face of the Arctic: Four decades of greening and implications for tundra ecosystems. Frontiers in Environmental Science, 13. https://doi.org/10.3389/fenvs.2025.1525574

Lacroix, F., Zaehle, S., Caldararu, S., Schaller, J., Stimmler, P., Holl, D., Kutzbach, L., & Göckede, M. (2022). Mismatch of N release from the permafrost and vegetative uptake opens pathways of increasing nitrous oxide emissions in the high Arctic. Global Change Biology, 28(20), 5973–5990. https://doi.org/10.1111/gcb.16345

Stocker, B. D., Dong, N., Perkowski, E. A., Schneider, P. D., Xu, H., de Boer, H. J., Rebel, K. T., Smith, N. G., Van Sundert, K., Wang, H., Jones, S. E., Prentice, I. C., & Harrison, S. P. (2025). Empirical evidence and theoretical understanding of ecosystem carbon and nitrogen cycle interactions. New Phytologist, 245(1), 49–68. https://doi.org/10.1111/nph.20178

Winkler, A. J., Myneni, R. B., Hannart, A., Sitch, S., Haverd, V., Lombardozzi, D., Arora, V. K., Pongratz, J., Nabel, J. E. M. S., Goll, D. S., Kato, E., Tian, H., Arneth, A., Friedlingstein, P., Jain, A. K., Zaehle, S., & Brovkin, V. (2021). Slowdown of the greening trend in natural vegetation with further rise in atmospheric CO2. Biogeosciences, 18(17), 4985–5010. https://doi.org/10.5194/bg-18-4985-2021
