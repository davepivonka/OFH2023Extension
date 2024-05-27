# OFH2023Extension

This code allows me to automatically replicate Table 1 in Orr, Fowler and Huber (2023): Is Affective Polarization Driven by Identity, Loyalty, or Substance?
It also allows extension of the table by separating the respondents into different sub-groups to be analysed separately. These sub-groups can be specified inside of the functions, and the code outputs LaTeX tables.

The main functions are `maketable1`, which is just as detailed as Table 1 in the article (with the optional separation into sub-groups), and `makedifftable`, which is useful to display results for multiple analyses in one table - only displays the most important information from the table, but separated on as many variables as needed.

The article that this code is based on: https://doi.org/10.1111/ajps.12796
Dataset by Dias and Leikes (2021), Study 2 on: https://doi.org/10.7910/DVN/JHJJW0
