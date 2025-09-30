# Immigrant Households
Replication code and analysis of American immigrant household configurations

### 📦️ Part A: Clone the repo and configure the R project

These steps will allow you to install the code on your computer that runs this project and set up the environment so that it mimics the environment on which the code was developed.

1. **Clone the repo**: Open a terminal on your computer. Navigate to the directory you would like to be the parent directory of the repo, then clone the repo.

    MacOS/Linux:
    
    ```cmd
    cd your/path/to/parent/directory
    ```
    ```cmd
    git clone https://github.com/lorae/immmigrant-households immigrant-households
    ```
    
    Windows:
    
    ```bash
    cd your\path\to\parent\directory
    ```
    ```bash
    git clone https://github.com/lorae/immigrant-households immigrant-households
    ```

2. **Open the R project**: Navigate into the directory, now located at `your/path/to/parent/directory/immigrant-households`.
Open `immigrant-households.Rproj` using your preferred IDE for R. (We use R Studio.)

    Every subsequent time you work with the project code, you should always open the `immigrant-households.Rproj` file
    at the beginning of your work session. This will avoid common issues with broken file paths or an incorrect working directory.

3. **Initialize R environment**: Install all the dependencies (packages) needed to make the code run on your computer. 
Depending on which packages you may have already installed on your computer, this setup step may take from a few minutes to over 
an hour.

    First, ensure you have the package manager, `renv`, installed. Run the following in your R console:
    
    ```r
    install.packages("renv") # Safe to run, even if you're not sure if you already have renv
    ```
    ```r
    library("renv")
    ```
    
    Then initialize the project:
    
    ```r
    renv::init()
    ```
    
    At this point, a message will print into the console informing you that this project already has a lockfile. 
    Select option `1: Restore the project from the lockfile`. 
