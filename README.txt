Dataset title: Individual predator DNA metabarcoding-based predator-prey interactions and isotopic niches from Palmyra Atoll 2009-2017

###

Abstract: These are data and code associated with the terrestrial top and intermediate predator diets from Palmyra Atoll (2009-2017). These data include both stable isotope and diet DNA metabarcoding data for top predators (Araneae: Heteropoda venatoria) and diet DNA metabarcoding data for intermediate predators (Araneae: Neoscona theisi, Scytodes longipes, Keija mneon). These datasets include the final compiled datasets for diet DNA data (originally from Miller-ter Kuile et al. 2022) and raw data for stable isotopes for top predators. The code includes code to reproduce all data cleaning, merging, statistical analyses, and visualizations. 

###

Creators: Miller-ter Kuile, Ana, A.. Apigo, A. Bui, K. Butner, J. N. Childress, S. Copeland, B. P. DiFiore, E. S. Forbes, M. Klope, C. I. Motta, D. Orr, K. A. Plummer, D. L. Preston, H. S. Young

###

Contact: Ana Miller-ter Kuile (ana.miller.ter.kuile@gmail.com)

###

Other Personnel: Field Crew: A. Briggs, C. Bunriske, M. Degraff, P. DeSalles, M. Espinoza, E. Hoffman, T. Jen, J. McLaughlin, N. Wenner, E. Wulczyn
Lab Technician: Chelsea Steel, Emily Lutz, Tessa Chou, A. Carter

###

Keywords: Food chain, Araneae, diet DNA metabarcoding, stable isotope analysis

###

Funding of this work:
Hillary S. Young, National Science Foundation (DEB-1457371)
Rodolfo Dirzo, National Science Foundation (DEB-1457371); National Geographic Society (8574-08 & 9698-15)
Hillary S. Young, UC Santa Barbara Faculty Senate Grant
Ana Miller-ter Kuile, Stanford University School of Earth Sciences Summer Research Grant

###

Timeframe:
Begin Date: July 2009
End Date: April 2022

###

Geographic location: 
Verbal Description: Palmyra Atoll National Wildlife Refuge, Northern Line Islands
Coordinate: 5.883333
Coordinate: -162.08333

###

Taxonomic species or groups
Phylum: Arthropoda, Class: Arachnida, Order: Araneae;
1. Opopaea sp. (Family: Oonopidae) 
2. Neoscona theisi (Family: Aranaiedae), 
3. Heteropoda venatoria (Family: Sparassidae)
5. Scytodes longipes (Family: Scytodidae)

###

Methods: 
Field site and collections
We conducted this work on Palmyra Atoll National Wildlife Refuge, Northern Line Islands (5º53’ N, 162º05’W). Palmyra Atoll has a well-characterized species list, and like many atolls, is relatively species poor, allowing for detailed characterization of potential diet items (Handler et al. 2007). Predator individuals used for diet DNA and stable isotope analyses were collected across two habitat types representative of "high" and "low" productivity (Pisonia grandis and Cocos nucifera, respectively; Young et al. 2010).

Isotope sample collection:
All individuals were collected individually and frozen. Then organisms for which only isotope data were derived, we used individual body parts (usually legs, but sometimes other body parts that did not include digested material). We initially froze samples and then they were dried at 55 degrees C, and finally ground into a powder. We submitted samples to the University of California Davis, Stable Isotope Facility (SIF, Davis, California, USA), which processes samples on a PDZ Europa ANCA-GSL elemental analyzer interfaced to a PDZ Europa 20-20 isotope ratio mass spectrometer (Sercon Ltd., Cheshire, UK). We corrected raw δ15N values using a mixing model specific to baseline sources on Palmyra Atoll (Young et al. 2013). This correction meant subtracting a δ15N_base from δ15N_consumer, following an equation to account for multiple baseline sources (marine and terrestrial in this system): 

δ15N_base = (δ15N_plants * alpha + δ15N_marine * (1-alpha)) / Delta. 

and 

alpha = (δ13C_consumer - δ13C_marine)/(δ13C_plant - δ13C_marine)

and 

Delta = 0.0034

DNA sample collection:

For diet DNA metabarcoding samples, we used a combination of methods, including individual collection during visual surveys for understory, and soil collections and canopy fogging with insecticide onto collection sheets for canopy individuals. All individuals were collected individually with sterilized implements (ethanol-burned forceps) in sterilized collection containers containing 95% EtOH to avoid contamination  (Greenstone et al. 2011). All individuals were stored in 95% EtOH at -20ºC before DNA extraction. 

DNA extraction, PCR amplification, library preparation, sequencing, and denoising

We individually measured the length of each predator (mm) and separated the thorax, opisthosoma, or trunk (depending on predator species, (Krehenwinkel et al. 2017, Macías-Hernández et al. 2018)) for DNA extraction following a modified CTAB extraction protocol (Fulton et al. 1995). While most individuals were run in separate samples (70%, n = 121/173), some individuals were too small to extract ample DNA from only one individual (mean size of 4.04 ± 0.12 mm in total length), and so we combined these individuals with other individuals from the same species, size range (within ± 0.5 mm in length), and sampling period. For these combined samples, we aimed for a minimum total sample weight of 5mg, and ideal sample weights of 10-20mg, a range we had previously determined to be sufficient for downstream DNA extraction and cleaning protocols. This resulted in a maxiumum of 12 individuals in one sample (SI Figure 6). Following methods in (Krehenwinkel et al. 2017), we standardized concentrations of 40uL of each sample to 20ng/ul and used Ampure XP (Agencourt, Beverly, MA, USA) beads to remove higher molecular weight predator DNA prior to PCR steps. We then amplified the CO1 gene, which is well-represented in online databases (Porter and Hajibabaei 2018) with general metazoan primers (mlCOIintF/Fol-degen-rev; (Yu et al. 2012, Leray et al. 2013, Krehenwinkel et al. 2017)). We ran total reaction volumes per sample of 25μL, with 9μL nuclease free water, 12.5μL GoTaq Green Master Mix (Promega Corp., Madison, WI, USA), 1.25μL of each primer (at 10mM), and 1μL of DNA template (at 10ng/μL) and ran a duplicate for each sample. We followed a PCR protocol as follows: 3 minutes at 95ºC, 35 cycles of: 95ºC for 30 seconds, 46ºC for 30 seconds, 72ºC for one minute; ending with 72ºC for five minutes. We removed reaction dimer with Ampure XP beads at 0.8x bead-to-DNA ratio. We then attached Illumina index primers (Nextera XT Index Kit v2) with 5μL of PCR product per reaction and the recommended PCR protocol for these primers (Illumina 2009). We combined and cleaned successfully amplified duplicate samples using Ampure XP beads (0.7x beads-to-DNA) and diluted each sample to 5nM in 10mM TRIS, using 1uL of each sample for sequencing. For sequencing runs, we multiplexed all samples along with one negative control and two PCR4-TOPO TA vectors (Invitrogen, Carlsbad, CA, USA) containing the internal transcribed spacer 1 region from two fungal species as positive controls (GenBank accession numbers: MG840195 and MG840196;  (Toju et al. 2012, Clark et al. 2016, Apigo and Oono 2018)). We submitted multiplexed samples for sequencing at the University of California, Santa Barbara Biological Nanostructures Laboratory Genetics Core. Samples were run on an Illumina MiSeq platform (v2 chemistry, 500 cycles, paired-end reads) with a 15% spike-in of PhiX. Following sequencing, samples were demultiplexed using Illumina’s bcl2fastq conversion software (v2.20) at the Core facility.  
We merged, filtered (max ee  = 1.0), and denoised (clustered) our sequences around amplicon sequence variants (ASVs) using the DADA2 algorithm in R (dada2 package version 1.1.14.0; Callahan et al., 2016). Prior to denoising with DADA2, we used cutadapt (version 1.18, (Martin 2011)) to remove primers from each sequence. We removed samples from analysis that had not been sequenced to sufficient depth using iNEXT (Hsieh et al. 2016) and a lower quantile cutoff. We rarefied remaining samples (McKnight et al. 2019) based on the sample with the lowest sequencing depth which had been sequenced with 95%+ sampling completeness based on iNEXT (version 2.0.20) interpolation and extrapolation methods (Hsieh and Chao 2017). We rarefied using the rrarefy() function in the vegan (version 2.5.6) package in R to 15,954 reads per sample.

ASV taxonomic assignment with BLAST and BOLD
From the output of the DADA2 algorithm, we created a list of unique ASVs which we matched to taxonomies both in the GenBank and BOLD databases. For GenBank, we used BLAST (version 2.7.1) with the blastn command for taxonomic assignment of each ASV using the computing cluster at UC Santa Barbara, comparing against the GenBank nucleotide database with an evalue of 0.01 (downloaded on November 20, 2019). We visualized and exported taxonomic alignment using MEGAN Community Edition (version 6.18.0, (Huson et al. 2016)), using default settings and selecting the subtree with all possible diet items for this species (Kingdom: Animalia, Clade: Bilateria). For BOLD taxonomic assignment, we used the BOLD IDEngine of the CO1 gene with Species Level Barcode Records (accessed May 21, 2020; 4,070,029 Sequences, 225,114 Species, and 104,607 Interim Species in database) to match each ASV list to taxonomies. We combined taxonomic assignments from both programs and discarded taxonomic assignments that were mismatched at the family level or higher (Elbrecht et al. 2017). We chose to combine prey taxonomies at the order level by summing the cumulative read abundances across the ASVs that corresponded to each diet order in each sample. We corrected for potential sequence jumping (‘cross-talk’) across samples by removing reads across samples that emerged in negative controls (Oono et al. 2020) and all DNA matching any predator family present on an individual sequencing run was removed as a conservative method to account for potential sequence jumping (‘cross-talk’) (van der Valk et al. 2020). We verified ASV specificity based on positive control samples.

Data analyses:

To examine how stable isotope-based trophic niche shifts with environmental context, we calculated two common trophic niche metrics (standard ellipse area: Layman et al. 2012, kernel utilization density: Eckrich et al. 2019). We calculated the 95% confidence interval for both metrics and used a generalized linear model to examine how habitat context shapes isotopic niche space. We also examined how each isotopic signature (𝛿13C and 𝛿15N) shifted individually with environmental context using a set of linear mixed effects models. We used Gaussian error distributions for all linear models and random effects of islet and year to account for spatial and temporal non-independence. All models included abiotic context (categorical variable: high vs. low productivity) as fixed effects (n = 88 individuals from high-; 64 from low-productivity habitats).

To examine how diet DNA shifts with habitat context for both top and secondary predators, we determined shifts in DNA diet niche (beta diversity) between the two environmental contexts using distance based redundancy analyses (Jupke and Schafer 2020). We ran one model for each predator category (n = 21 and 13 individuals for the top predator species in high- and low-productivity habitat, respectively; n = 23 and 8 secondary predators from each habitat, respectively) and used the Jaccard dissimilarity index based on the presence-absence nature of our data. In the event of dissimilarity in diet composition with environmental context (p-value <= 0.05), we determined whether dissimilarity (beta diversity) was based on turnover (shifting to new prey items) or nestedness (one prey community is a subset of the other). 

We ran all statistical analyses in R (version 4.0.2; R Core Team 2020) and cleaned data with the here (version 1.0.1, Müller 2020) and tidyverse packages (version 1.3.0, Wickham et al. 2019). We computed isotopic niches using the rKIN package (version 0.1, Albeke 2017), ran mixed effects models in the glmmTMB package (version 1.1.2, Brooks et al. 2017), and ran model diagnostics using the DHARMa (version 0.3.3, Hartig 2020) and effects (version 4.2-0, Fox 2003) packages. We ran distance based redundancy analyses using the vegan (version 2.5-7, Oksanen et al. 2020) and betapart (version 1.5.4, Baselga et al. 2021) packages. 

###

Data Provenance:

Some of the data in this dataset are initially analyzed here. Others come from these two studies:

Miller‐ter Kuile et al., “Predator–Prey Interactions of Terrestrial Invertebrates Are Determined by Predator Body Size and Species Identity.”

Young et al., “The Roles of Productivity and Ecosystem Size in Determining Food Chain Length in Tropical Terrestrial Ecosystems.”

###

Data Files:

Subfolder:DNA
Table name: all_prey_DNA.csv
Table Description: This includes all predator-prey interaction pairs from diet DNA, including taxonomic information on the predator/consumer and on the prey/diet item

Column name | Description | Unit or code explanation or date format | missing value code

ASV | The ASV id assigned to the diet sequence | NA | NA

sample | The predator sample ID corresponding to this interaction | NA | NA

reads | the number of reads assigned to that ASV in that sample | NA | NA

pred_Class | the class of the predator/consumer | NA |NA

pred_Order | the order of the predator/consumer | NA | NA

pred_Family | the family of the predator/consumer | NA | NA

pred_Genus | the genus of the predator/consumer | NA | NA

pred_Species | the species of the predator/consumer | NA | NA

pred_ID | an identifier given to each predator in the field | NA | NA

sample_str | A sample string ID used to group predators of the same species | NA | NA

Domain | The domain of the prey/diet item | NA | no assignment matched

Phylum | The phylum of the prey/diet item | NA | No assignment matched

Class | The class of the prey/diet item | NA | No assignment matched

Order | The order of the prey/diet item | NA| No assignment matched

Family | The family of the prey/diet item | NA | No assignment matched

Genus | The genus of the prey/diet item | NA | No assignment matched

Species | The species of the prey/diet item | NA | No assignment matched

run | the sequencing run ID from the initial study | NA | NA

Table name: Sample_metadata.csv
Table Description: information on the collection sites, methods, and other data associated with each predator/consumer sample

Column name | Description | Unit or code explanation or date format | missing value code

Method | Collection method for predator/consumer individual | NA | NA

Island | The islet on Palmyra on which the sample was collected | NA | NA

Habitat | The vegetation dominant in the area where the sample was collected | PG = Pisonia Grandis; CN = Cocos nucifera; TA = Tournefortia argentea; PF = Pandanus fischeranus; TC = Terminalia catalpa | NA | NA 

Microhabitat | a specific microhabitat in which the sample was collected, such as soil, canopy, or understory | NA | NA

Year | the year the sample was collected | YYYY | NA

Order | The order of the predator/consumer | NA | NA

ID | A field ID assigned to each morphospecies | NA | NA 

Date Collected | the date the sample was collected | M/DD/YY | NA 

Extraction ID | An ID corresponding to the ID for diet DNA extraction samples | NA | NA

Extr_ID2 | the same as the previous column but with leading zeros | NA | No additional ID needed

No. individuals | the number of individuals in each sample | NA | NA

Length_mm | the length (or average length) of the consumer(s) | millimeters | no measurement taken


Isotope_ID | An ID given for isotope samples for predators with both DNA and isotope samples | NA | No isotope sample collected for the predator

Subfolder: environmental

Table name: Island_data.csv
Table Description: Information on the productivity and area of each islet in this study

Column name | Description | Unit or code explanation or date format | missing value code

Island | The name of the islet | NA | NA

Island_Area | The area of the islet | square meters | NA

Island_prod | the soil productivity of each islet | grams of nitrogen per meter squared per day | No data collected for the islet

Subfolder: isotopes

baseline:

Table name: 
Table Description: 

### 

Scripts/code (software)

