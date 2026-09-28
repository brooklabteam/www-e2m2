# www-e2m2

### Basics

**www-e2m2** is a [Jekyll](https://jekyllrb.com/)-generated static site. The published site is deployed using [Netlify](https://netlify.com) with a domain registered via [Cloudflare](https://www.cloudflare.com).

[![Netlify Status](https://api.netlify.com/api/v1/badges/6115606f-755e-4d93-8662-a7bb0d56d742/deploy-status)](https://app.netlify.com/sites/e2m2/deploys)

### Information architecture

- Index
- Acknowledgements
- Archives
  - 2018 ~> course syllabus, etc
  - 2019
  - 2020
  - 2022
  - 2024
  - ...
  - present year
- assets
  - img ~> any images/marketing materials
  - 2018 ~> all course material for this year's course
  - 2019
  - 2020
  - 2022
  - 2024
  - ...
  - present year
- Preparation
- Syllabus

### Repo TODOs

- **`assets/2025/Tutorials/intro_phylogenetics_tutorial/` vs `intro_phylogenetics_tutorial_lemur/`**: these two directories overlap heavily and look like duplicates of the same phylogenetics tutorial. `intro_phylogenetics_tutorial/` also contains an extra self-nested `intro_phylogenetics_tutorial/intro_phylogenetics_tutorial/` copy (a leftover unzip artifact), while `intro_phylogenetics_tutorial_lemur/` appears to be the more complete version (it has the finished RAxML tree output files the other is missing). Needs a decision on which is canonical, then: collapse to one directory, remove the redundant nesting, and update any page that links to it.
