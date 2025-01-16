<p align="center">
    <img src="https://user-images.githubusercontent.com/15319503/195926656-9d3d37b5-86ab-4d4b-9e6d-3c70d5399c73.png" alt="disaster" width="150">
</p>


<p align="center">
    <h1 align="center">MultiHazard-Shiny App</h1>
</p>
  
<p align="center">
    <a href="https://sfwmdcompoundevents.shinyapps.io/MultiHazard_App/">
        <img src="https://img.shields.io/badge/Hosted_App-Live-brightgreen?style=for-the-badge" alt="Hosted App">
    </a>
    <a href="https://github.com/rjaneUCF/MultiHazard">
        <img src="https://img.shields.io/badge/MultiHazard-Package-blue?style=for-the-badge" alt="MultiHazard R Package">
    </a>
    <a href="https://opensource.org/licenses/MIT">
        <img src="https://img.shields.io/badge/License-MIT-blue.svg?style=for-the-badge" alt="License: MIT">
    </a>
</p>


## 🌟 Overview

The **MultiHazard-Shiny App** is an interactive web application for conducting bivariate joint probability analyses. Powered by the [MultiHazard R package](https://github.com/rjaneUCF/MultiHazard), this application provides a user-friendly interface for conducting advanced statistical analyses on time series data, supporting researchers and practitioners to explore the relationships between hydrological and meteorological variables, aiding in the assessment of multi-hazard risks such as compound flooding.


## 🛠 Key Features

- **Interactive Analysis**: Perform bivariate joint probability analysis with an intuitive Shiny interface.
- **Flexible Data Support**: Analyze time series data with a `Date` class date/time column.
- **Data Compatibility**: Works seamlessly with rainfall, water levels, and other hazard-related datasets.
- **Visualization**: Generate clear and concise visual outputs to aid in decision-making.
- **Example Dataset**: Includes example time series of rainfall (Miami Airport) and water levels (coastal control structure S-22) in the `Data` folder.

## 🔗 Quick Links

- **[MultiHazard R Package](https://github.com/rjaneUCF/MultiHazard)**: Core package providing statistical tools and methods.
- **[Hosted App](https://sfwmdcompoundevents.shinyapps.io/MultiHazard_App/)**: Access the live application.
- **[Shiny App Repository](https://github.com/rjaneUCF/MultiHazard-Shiny)**: Explore the codebase for the Shiny app.


## 📦 Installation & Usage

### Option 1: Online Access
- **Hosted App**: Access the app directly through this link: [MultiHazard Shiny App](https://sfwmdcompoundevents.shinyapps.io/MultiHazard_App/)

### Option 2: Local Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/rjaneUCF/MultiHazard-Shiny.git
   cd MultiHazard-Shiny
   ```

2. Install the required R packages:
   ```R
   install.packages(c("shiny", "MultiHazard", "ggplot2", "dplyr", "gamlss", "gamlss.mx", "fitdistrplus"))
   ```

3. Launch the Shiny app locally:
   ```R
   shiny::runApp()
   ```


## 📊 Example Dataset

The repository includes example data files located in the `Data` folder, containing:
- Rainfall data from **Miami Airport**: `Miami_Airport_Rainfall_df.csv`.
- Water level data at the **S-22 coastal control structure**: `S22_Tailwater_df.csv`.

### Notes:
- **Rainfall Detrending**: We recommend **not detrending rainfall data** before analysis.
- Ensure the `date/time` column in your dataset is of class `Date` for compatibility.


## 📝 Citation

If you use this app in your work, please cite the MultiHazard package and any associated publications. Proper citation supports the development of tools like this one!

>Jane, R., Cadavid, L., Obeysekera, J., and Wahl, T. (2020). Multivariate statistical modelling of the drivers of compound flood events in South Florida, Nat. Hazards Earth Syst. Sci., 20, 2681–2699, https://doi.org/10.5194/nhess-20-2681-2020.

## 🤝 Contributing

We welcome contributions! Whether it’s fixing bugs, suggesting new features, or improving documentation, your input is valuable. Please submit a pull request or open an issue on the [GitHub repository](https://github.com/rjaneUCF/MultiHazard-Shiny).

## 📄 License

This project is licensed under the [MIT License](LICENSE).

## 💡 Contact

For questions or feedback, please contact the repository maintainers.

---

<p align="center">
    <strong>Empowering informed decision-making through robust hazard analysis.</strong>
</p>





