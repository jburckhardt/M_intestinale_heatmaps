# Heatmaps for *M. intestinale* Strains 

**link to a running instance of your Shiny app:**

Old version of the APP:
http://jburckhardt.shinyapps.io/M_intestinale_heatmaps

**New** version of the APP (Assignment B4):
https://jburckhardt.shinyapps.io/M_intestinale_heatmapsV2/

**Choice of assignment:** 

Assignment 4-C - improve Shiny app.

**A description of your app**

*Muribacul instestinale* (*M. intestinale*) is a bacterial family that is part of the bacteria that live within the human gut. Although *M. intestinale* 
is culturable in the lab, we still do not understand many of its genetic functions. There are many isolated strains of *M. intestinale*, but we have been unable to
really compare and constrast their genomes. Thus, using sequencing data for different *M. intestinale* strains obtained in my lab (Tropini lab, unpublished data), I 
aimed to annotate the genomes of these strains and compared them. To annotate the genomes, I used the RAST (Rapid Annotation using Subsystem Technology) web server,
which predicts annotations (genes) from a given genome sequence. Once I had the annotations from RAST, I used the output to join the annotation data into one table
(`S24-7_data.csv`). This table is structured as follows:

|Category | Subsystem | Subcategory | Strain1 | Strain2 | ... | StrainN |
| ------- | --------- | ----------- | ------- | ------- | --- | ------- |
| Gene Category 1 | Gene Subcategory 1A| Gene Subsystem 1A.1 | 0 | 2 | ... | X |
| Gene Category 1 | Gene Subcategory 1A| Gene Subsystem 1A.2 | 2 | 1 | ... | X |
| Gene Category 1 | Gene Subcategory1 B| Gene Subsystem 1B.1 | 4 | 4 | ... | X |
| Gene Category 2 | Gene Subcategory 2A| Gene Subsystem 2A.1 | 1 | 1 | ... | X |
| Gene Category 2 | Gene Subcategory 2B| Gene Subsystem 2B.1 | 0 | 1 | ... | X |
| Gene Category 2 | Gene Subcategory 2B| Gene Subsystem 2B.2 | 5 | 5 | ... | X |

Where:

- **Category** = A broad classification that group genes with 'simmilar' functions.
- **Subcategory** = gene groups withing once gene category. More specific simmilarities between genes.
- **Subsystem** = A more specific grouping of genes with 'similar' functions within one Subcategory of genes.
- **Strain1-N** = The number of genes found within a **subsystem** for that specific strain.

This app allows the user to interactively compare the different gene subsystems found in M. intestinale in a visual format (using Heatmap). 
This app facilitates finding patterns and differences in gene systems for different M. intestinale strains, which can lead to the creation of new
hypotheses surrounding this unexplored bacterium. 

Currently, the app has the following features:

1. Creates an interactive plotly heatmap that allows the user to zoom-in/out into specific parts of the heatmap and, when hovering over a cell of the heatmap, the value (number of genes) for that specific heatmap cells is shown in a pop-up.
2. Checkboxes thta allow the user to select which M. intestinale strains they want to include in the heatmap
3. Drow-down menu where the user can select which specific Gene category they want to project in the heatmap.
4. Dynamic switches that when the user turn 'on', they can further filter/select the data that is shown in the heatmap based on Subcategory or Subsystem
5. Input text boxes where the user can assign the colors that will be used for the heatmap! (they can assign the lowest and highest color values)
6. A download buttom that allows the user to save the heatmap as a .png file.
7. A 'Table' tab in the sidebar menu that directs the user to a rendered table of the values present in the Heatmap. The table can also be downloaded as a .csv file

