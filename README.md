# Heatmaps for *M. intestinale* Strains 

**link to a running instance of your Shiny app:**

http://jburckhardt.shinyapps.io/M_intestinale_heatmaps

**Choice of assignment:** 

Choice B - own Shiny app.

**A description of your app**

*Muribacul instestinale* (*M. intestinale*) is a bacterial family that is part of the micrboes that live within the human gut. Although *M. intestinale* 
is culturable in the lab, we still do not understand many of its genetic functions. There are many isolated strains of *M. intestinale*, but we have been unable to
really compare and constrast their genomes. Thus, using sequencing data for different *M. intestinale* strains obtained in my lab (Tropini lab, unpublished data), I 
aimed to annotate the genomes of these strains and compared them. To annotate the genomes, I used the RAST (Rapid Annotation using Subsystem Technology) web server,
which predicts annotations (genes) from a given genome sequence. Once I had the annotations from RAST, I used the output to join the annotation data into one table
(`S24-7_subsystems.csv`). This table is structured as follows:

|Category | Subsystem | Strain1 | Strain2 | ... | StrainN |
| ------- | --------- | ------- | ------- | --- | ------- |
| Gene Category 1 | Gene Subsystem A | 0 | 1 | ... | 3 |
| Gene Category 1 | Gene Subsystem B | 0 | 3 | ... | 5|
| Gene Category 2 | Gene Subsystem C | 1 | 1 | ... |  6|

Where:

- **Category** = A broad classification that group genes with 'simmilar' functions.
- **Subsystem** = A more sprecific grouping of genes with 'similar' functions.
- **Strain1-N** = The number of genes found for the specific subsystem for the specific strain.

This app allows the user to interactively compare the different gene subsystems found in M. intestinale in a visual format (using Heatmaps). 
This app facilitates finding patterns and differences in gene systems for different M. intestinale strains, which can lead to the creation of new
hypotheses surrounding this unexplored bacterium. Currently, the app only allows the user to filter base on gene category and select strains of interest. However, many other functionalities can be added to it in the future! 
