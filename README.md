# 👨‍👩‍👧 Immigrant Households
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
    
### 📥️ Part B: Download raw data from IPUMS USA

The [IPUMS Terms of Use](https://www.ipums.org/about/terms) precludes us from directly sharing the raw microdata extract, however,
the data used in this analysis is freely available and simple to download after setting up an IPUMS USA account. In this step,
we explain this process and how to "order" a data extract that exactly matches the one used in this study.

4. **Copy the file** `example.Renviron` to a new file named `.Renviron` in the project root directory. 
You can do this manually or use the following terminal commands:

    MacOS/Linux:
    
    ```bash
    cp example.Renviron .Renviron
    ```
    
    Windows:
    
    ```cmd
    copy example.Renviron .Renviron
    ```
    
5. **Set up your IPUMS USA API key**: If you don't already have one, set up a free account on 
[IPUMS USA](https://uma.pop.umn.edu/usa/user/new). Use the new account to login to the 
[IPUMS API Key](https://account.ipums.org/api_keys) webpage. Copy your API key from this webpage.

6. **Open `.Renviron`** and replace `your_ipums_api_key` with your actual key.  Do not include quotation marks. 
R will automatically load `.Renviron` when you start a new session. This keeps your API key private and separate 
from the codebase.

    🛑 Important: `.Renviron` is listed in `.gitignore`, so it will not be tracked or uploaded to GitHub — but `example.Renviron` is tracked, so do not put your actual API key in the example file.
