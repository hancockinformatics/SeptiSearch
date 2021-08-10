
# SeptiSearch Tutorial

## Table of Contents

1. [Explore the Collection by Study](#explore-the-collection-by-study)
2. [Visualize the Top-Occurring Molecules](#visualize-the-top-occurring-molecules)
3. [Perform GSVA with Sepsis Signatures](#perform-gsva-with-sepsis-signatures)
4. [Perform Pathway Enrichment](#perform-pathway-enrichment)


## Expore the Collection by Study

The first tab allows you to easily browse and search the data we've curated. The
first field in the sidebar allows you to enter search terms to filter the 
entries based on the title of study/article. For example, you could enter 
"COVID" to display all studies we've gathered with a focus on COVID-19 
infections in the context of sepsis.  

The second field facilitates searching our data for specific molecules, and will
display any studies whose signature includes the molecules you enter. Note that
this searching is case-sensitive, but does allow for partial matching of names 
(e.g. entering "S100" will return entries including S100A8, S100A9, etc.).  

The final field allows you to filter the table for a specific omics type, namely
either Transcriptomics or Metabolomics.  

If, after entering a number of searches or filter criteria, you would like to
return to the original table, simply click the "Restore defaults" button at the 
bottom of the sidebar, which will reset all inputs. In some cases it may also
help to simply refresh the page from your browser.  

The table on the right will display all matching studies based on the specified
filters - intially it simply lists all studies curated in SeptiSearch. Clicking 
on a row will bring up a second table listing all entries (i.e. molecules) for 
that particular study. Multiple rows can be selected to list molecules from a 
number of studies simultaneously. A small seach box just above this second table 
(on the right-hand side) will allow to to search all columns/rows of the 
study-specific table. For example, you click on the entry for "*A single-cell 
atlas of the peripheral immmune response...*" in the top table, then use the 
lower search box to look for S100 molecules within that signature, returning 10
entries from the total 231 molecules. Once you have click on one or more rows 
(i.e. created the lower table), you can use the provided button to download
the lower table as a plain text, tab-delimited table, which can be easily viewed 
in Excel or a similar program.


## Visualize the Top-Occurring Molecules

The second tab provides a more visual way of search the curated signatures, with
a greater focus on the molecules collected in SeptiSearch.  

The plot shows the most common molecules in the dataset for the given timepoint.
The sidebar on the left provides a number of ways to filter the data, which will
update the plot to show the top-occurring molecule which matches the specified
criteria. Again, a "Restore defaults" button at the bottom of the sidebar will 
reset all inputs and return the plot to its original state.  

At any time, you can click one of the bars (molecules) in the plot to show all 
entries for that molecule in SeptiSearch. This table can also be downloaded
using the button at the bottom of the sidebar.


## Perform GSVA with Sepsis Signatures

One of the great features of SeptiSearch is the ability to apply our curated 
sepsis signatures to your own data using Gene Set Variation Analysis (GSVA).
This method identifies sets of genes (in our case, the sepsis signatures) within
expression data produced by technologies such as RNA-Seq or microarrays. Each
sample within your dataset is assigned an enrichment score for each gene set, 
which can be positive or negative relative to the other samples in your data.
For example, you may be able to identify that certain sepsis signatures are
expressed more highly in certain treatment groups than others.  

To help demonstrate the usefulness of GSVA, we provide an example dataset you
test out directly in the tab, all with a simple button press! By clicking 
"Load example data", a subset of the microarray data from GSE65682 (both the 
expression data and sample labels, a.k.a. metadata) is loaded by the app and 
made ready for testing. Just click the "Submit expression data for GSVA" button
at the bottom of the sidebar to see how the results look. 

If you'd like to submit your own data for GSVA, the expression component needs
to meet certain criteria, which are listed in the sidebar. To repeat them here:

- Your data must in a plain text, comma-delimited (CSV) file
- The first column must contain Ensembl gene IDs
- The remaining columns should contain samples, with unique names
- Values within the matrix should be transformed/normalized, as is appropriate
for your data/technology
	- We provide a link to information on the Variance
	Stabilized Transformaton (VST) which is often suitable for RNA-Seq data, but
	other methods may also be used

While you can submit only the expression data for GSVA, it's recommended to 
also include some metadata to make the final heatmap more informative. There
are a few requirements for submitted metadata:

- Your data must in a plain text, comma-delimited (CSV) file
- The first column should contain sample names, which must match the sample
names from your expression data
	- Any missing, extra, or non-matching sample names will generate an error
- All remaining columns will be considered variables/traits (e.g. disease 
status, condition, etc.)

Once you've uploaded either the expression data alone or expression and 
metadata, click the "Submit expression data for GSVA" to run the analysis and 
view the results.  

The resulting table can be downloaded with the button at the bottom of the
sidebar, and the image can be saved by right-clicking on it and selecting "Save
Image..." from the context menu.




## Perform Pathway Enrichment
