# twitterprofiles
Analysis of Twitter profiles in scholarly communication

Code works with R

This code assigns broad categories (science, communication, professional, personal, political, organizations, bots and publishers) to Twitter profiles descriptions based on keywords as RegEx - see dictionnary. This method accounts for the overlap of two or more categories in a description.

1) Load users descriptions (CSV, XLSX, etc.) and filter according to the purpose of the analysis

2) Clean data - step-by-step procedure to remove duplicates ; change capitals to lowercase ; remove urls ; remove emojis ; remove numbers ; remove punctuations ; remove NULL

3) Assign categories to Twitter description (RegEx based on keywords in English and French)

Link to dictionnary : https://figshare.com/articles/2nd_Codebook_users_-_Climate_change_research/8236598

  - sci (Science and Faculty) -> keywords related to scientific activities, such as academic professions ("professor"), higher ed ("students")
  - comm (Communication) -> keywords related to communication activities, such as "journalist", "writer", "producer"
  - organ (Organizations, Groups, Institutions) -> keywords related to organizations, such as "university", "alliance", "media"
  - bot (Bots, Accounts with machine generated content) -> keywords describing accounts that produce, at least partially, machine generated content or have automated activities, such as "news feed", "paper alerts"
  - pro (Professional) -> keywords about professionnal activities, such as "manager", "lawyer", "veterinarian"
  - pers (Personal) -> keywords about personal interests, such as "mom", "bike", "cats"
  - poli (Political) -> keywords about political activities or interests, such as "refugee", "advocate", "senator"
  - pub (Publishers) -> keywords about scientific publishers, such as "wiley", "sage", "plos one"
  
*Further expressions may be added to categories, and validity may be assessed by qualitatively reviewing the results through several passes ; the dictionnaries include general terms and need development and tweaking according to specific research needs ; new categories may be included in the future

Based on work by :
Alperin, J. P., Gomez, C. J., & Haustein, S. (2019). Identifying diffusion patterns of research articles on Twitter: A case study of online engagement with open access articles. Public Understanding of Science, 28(1), 2-18. https://doi.org/10.1177/0963662518761733

Côté, I. M., & Darling, E. S. (2018). Scientists on Twitter: Preaching to the choir or singing from the rooftops?. FACETS, 3, 682-694. https://doi.org/10.1139/facets-2018-0002

Haustein, S. (2018). Scholarly Twitter Metrics. In W. Glänzel, H.F. Moed, U. Schmoch, & M. Thelwall (Eds.), Handbook of Quantitative Science and Technology Research, Springer. Retrieved from https://arxiv.org/abs/1806.02201v2

Toupin, R. & Haustein, S. (2018). 2nd Codebook for users categorization – Climate change research. Figshare. https://doi.org/10.6084/m9.figshare.8236598

Tsou, A., Bowman, T. D., Ghazinejad, A., & Sugimoto, C. R. (2015). Who tweets about science? In Proceedings of the 2015 International Society for Scientometrics and Informetrics (pp. 95–100). Istanbul, Turkey. 

Vainio, J. & Holmberg K. (2017). Highly tweeted science articles: who tweets them? An analysis of Twitter user profile descriptions. Scientometrics, 112(1), 345-366. https://doi.org/10.1007/s11192-017-2368-0 
