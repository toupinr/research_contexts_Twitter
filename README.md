# twitterprofiles
Analysis of Twitter profiles in scholarly communication

Code works with R

This code assigns broad categories (science, communication, professional, personal, political, organizations, bots and publishers) to Twitter profiles descriptions based on keywords as RegEx - see dictionnary. This method accounts for thr overlap of two or more categories in a description.

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
  
  4) Create plot with UpSetR
  
*Further expressions may be added to categories, and validity may be assessed by qualitatively reviewing the results through several passes ; the dictionnaries include general terms and need development and tweaking according to specific research needs ; new categories may be included in the future
