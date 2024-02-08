# Learning from Errors

Welcome to the Learning from Errors project repository! This project originated as my undergraduate honors thesis, combining my passions for cognitive psychology and data science. The aim of this project is to investigate the mechanisms through which individuals learn from errors using a combination of typical behavioral data collection and intensive cognitive computational modeling.

## Repository Layout

- **analysis**: This directory contains the original analysis of the behavioral results using mixed linear models. Additionally, it includes a subdirectory `maximum_likelihood` which holds the Python implementation of the ACT-R cognitive architecture. In this subdirectory, I built two models and calculated their fit to each person using the original cohort's data- these results are stored in `LLmodel1.csv`.
  
- **data**: Here, you'll find the collected behavioral data in different formats.
  
- **task_materials**: This directory holds the code to implement the task in PsyToolkit.

- **shiny_app**: This directory contains the files for the Shiny app, including `app.R`, `ui.R`, and `server.R`. Additionally, there's a `www` directory with figures that update with newly collected data, as well as the `index.html` file for webpage styling and user interaction.

## About the Project

The repository hosts a Shiny app that provides an interactive explanation of my research work. Users can engage with the app to understand the concepts and even participate in the task themselves to see the results firsthand.

## Features

- **Interactive Shiny App**: Explore the core concepts and methodologies of the research through an engaging user interface.
  
- **User Participation**: Users can directly engage in the task to experience the learning process and observe their own results.
  
- **Statistical Modeling**: The results are generated through a combination of mixed linear models and maximum likelihood estimation, leveraging individualized data science techniques to delve into how specific individuals learn.

## Why This Project?

This project serves multiple purposes:

- **Education**: It offers an accessible way for individuals to learn about cognitive psychology and data science concepts.
  
- **Democratizing Research**: By making my research accessible through an interactive app, I aim to break down barriers and make scientific exploration more inclusive.
  
- **Demonstration of Skills**: The development of this app showcases my abilities in R, Python, and overall data science skills, highlighting my capability to merge different domains into a cohesive project.

## How to Use

To explore the Shiny app:

1. Clone this repository to your local machine.
  
2. Navigate to the directory containing the app files.
  
3. Run the Shiny app using RStudio or the command line.
  
4. Follow the on-screen instructions to engage with the app and explore the research findings.

## Feedback and Contributions

Feedback and contributions are welcome! If you have suggestions for improvements or would like to contribute to the project, feel free to open an issue or submit a pull request.

Thank you for visiting the Learning from Errors repository!
