**HEMOCM** is an interactive, web-based educational application built with the R Shiny framework. Designed to teach complex hematological diagnostics (cytology, flow cytometry, cytogenetics, and molecular biology), the application utilizes a retro DOOM-style First-Person Shooter (FPS) interface to maximize learner engagement and optimize memory retention.

## 🧬 Scientific Background
This application was developed as a pedagogical tool directly applying **Cognitive Load Theory**:
* **Managing Intrinsic Load:** The diagnostic difficulty progressively increases across a 20-level "dungeon," moving from basic cytology to complex molecular profiles.
* **Reducing Extraneous Load:** The interface completely avoids the "split-attention effect" by centralizing all relevant information (clinical query, options, target) into a single, cohesive visual field.

## 🚀 Features
* **Progressive Difficulty:** 20 levels of increasing diagnostic complexity.
* **Dynamic Algorithm:** Distractor answers are procedurally generated from the same disease category to force active clinical differentiation.
* **Gamification:** Health points, armor, unlockable weapons (Machine Gun, Blue Laser, Bazooka), and an achievement/trophy system.
* **Performance Tracking:** Generates a personalized statistical report highlighting specific clinical weaknesses for targeted self-correction.

## 🛠️ Installation & Usage

To run this application locally on your machine, you need [R](https://cran.r-project.org/) and [RStudio](https://posit.co/download/rstudio-desktop/) installed.

1. **Clone or download the repository:**
   Download the ZIP file and extract it, or clone via terminal:
   `git clone https://github.com/YOUR-USERNAME/HEMOCM-app.git`

2. **Install required R packages:**
   Open RStudio and run the following command to install the necessary dependencies:
```R
   install.packages(c("shiny", "bslib", "dplyr", "ggplot2"))# HEMOCM-app
