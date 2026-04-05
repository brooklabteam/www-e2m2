# CLAUDE.md — E2M2 Website

## Overview

This is the [E2M2](https://e2m2.org) website — a Jekyll static site deployed via Cloudflare Pages, built on the same base structure as the [Brook Lab website](https://brooklab.org).

Refer to the [brooklabteam/www CLAUDE.md](https://github.com/brooklabteam/www/blob/main/CLAUDE.md) for shared documentation on:
- Local development setup
- Repository structure conventions
- Adding news posts
- Navigation editing
- Site configuration
- Deployment via Netlify and GitHub Actions CI

## E2M2-Specific Differences

**Site config:** `_config.yml` sets `title: E2M2`, `url: https://e2m2.org`. The `_redirects` file (Netlify redirects) is included via `include:` in `_config.yml`.

**No `_posts/` directory:** This site does not use the Jekyll blog/news post system.

**Year-based asset folders:** Past event materials are organized under `assets/` by year (e.g., `assets/2024/`, `assets/2025/`).

**Top-level pages:**

| File | URL | Notes |
|---|---|---|
| `index.md` | `/` | `layout: home` |
| `syllabus.html` | `/syllabus` | Current year syllabus |
| `preparation.md` | `/preparation` | Pre-course preparation |
| `acknowledgements.md` | `/acknowledgements` | |
| `archives/` | `/archives` | Past year content |
| `planning_e2m2/` | — | Internal planning docs (not served as pages) |
