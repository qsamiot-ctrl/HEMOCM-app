# 🩸 HEMOCM: Hematology Survival Simulator

**HEMOCM** is an interactive, web-based educational application built with the R Shiny framework. Designed to teach complex hematological diagnostics (cytology, flow cytometry, cytogenetics, and molecular biology), the application utilizes a retro style First-Person Shooter (FPS) interface to maximize learner engagement and optimize memory retention.

## 🧬 Scientific Background
This application was developed as a pedagogical tool directly applying **Cognitive Load Theory**:
* **Managing Intrinsic Load:** The diagnostic difficulty progressively increases across a 20-level "dungeon," moving from basic cytology to complex molecular profiles.
* **Reducing Extraneous Load:** The interface completely avoids the "split-attention effect" by centralizing all relevant information (clinical query, options, target) into a single, cohesive visual field.

## 🚀 Features
* **Progressive Difficulty:** 20 levels of increasing diagnostic complexity.
* **Dynamic Algorithm:** Distractor answers are procedurally generated from the same disease category to force active clinical differentiation.
* **Gamification:** Health points, armor, unlockable weapons (Machine Gun, Blue Laser, Bazooka), and an achievement/trophy system.
* **Performance Tracking:** Generates a personalized statistical report highlighting specific clinical weaknesses for targeted self-correction.

## 🛠️ Installation & Usage (Easy Setup)

To run this application locally on your machine, you will need [R](https://cran.r-project.org/) and [RStudio](https://posit.co/download/rstudio-desktop/) installed.

### Step 1: Download the Application
**Option A: ZIP Download**
1. Click the green **"<> Code"** button at the top right of this page and select **"Download ZIP"**.
2. **Important:** Extract (unzip) the downloaded folder completely to a location on your computer (e.g., your Desktop). *Do not try to run the files directly from inside the ZIP viewer.*

**Option B: Git Clone**
```bash
git clone [https://github.com/YOUR-USERNAME/HEMOCM-app.git](https://github.com/YOUR-USERNAME/HEMOCM-app.git)
```
*(Note: Replace `YOUR-USERNAME` with your actual GitHub username).*

### Step 2: Install Required Packages
Open **RStudio**, paste the following command into the Console (the bottom-left window), and press **Enter**:
```R
install.packages(c("shiny", "bslib", "dplyr", "ggplot2"))
```

### Step 3: Launch the Game in RStudio
To ensure the application properly loads all its visual assets (monsters, weapons, backgrounds), you must set the correct working directory before playing. Follow these exact steps:

1. In RStudio, go to the top menu and select **File > Open File...**
2. Navigate to your newly extracted `HEMOCM-app` folder and open the **`app.R`** file.
3. **Crucial Step:** Go to the top menu again and click **Session > Set Working Directory > To Source File Location**. *(This tells RStudio exactly where to look for the `www` folder containing the game's images).*
4. Finally, look at the top right of the code editor window and click the green play button labeled **"Run App"**. 

## 📝 Citation & Authors
If you use HEMOCM in your research or teaching, please reference the associated publication:

> Amiot, Q., Assadi-Gazvini, C., & Locher, L. (202X). *HEMOCM: A Procedurally Generated, Gamified Web Application for Mastering Complex Hematologic Classifications*.

**Author Contributions:**
* **Quentin Amiot:** Conceptualized the application and supervised the coding process.
* **Lucy Locher:** Co-conceptualized the application framework.
* **Clara Assadi-Gazvini:** Reviewed the medical content and ensured scientific accuracy.

## 📜 License
This project is open-source and available under the [GPL-3.0 License](LICENSE).
