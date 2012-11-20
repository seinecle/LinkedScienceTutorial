# 1. Install R 2.15!, RStudio, and Gephi
If the Setup directory is missing, you have to download it separately.
You can find information about how to download in the file Setup_MISSING.md
### OS X 
    open Setup/R/R-2.15.1-signed.pkg
    open Setup/RStudio/RStudio-0.96.331.dmg 
    open Setup/Gephi/gephi-0.8.1-beta.dmg 

### Windows:
    .\Setup\R\R-2.15.1-win.exe
    .\Setup\RStudio\RStudio-0.96.331.exe
    .\Setup\Gephi\gephi-0.8.1-beta.setup.exe 

### Ubuntu:
SEE THE INSTRUCTIONS AT THE BOTTOM OF THIS README FILE

# 2. Install R packages
### OS X, Linux, etc.:
Start R.

    install.packages(c('SPARQL','sp','spacetime','ggmap','mapproj','igraph','network','ergm','zoo','gsubfn','rjson'),dependencies=TRUE)

### Windows:
Start RStudio.

    install.packages(choose.files(), repos=NULL)

Pick *.zip in the directory "Setup\R packages for Windows"

### OS X (without internet):
Start RStudio

    install.packages(Sys.glob("[path to tutorial]/Setup/R packages for OS X/*.tgz"), repos=NULL)

# 2. Run Fuseki
### OS X, Linux, etc.:
    cd Tutorials/jena-fuseki-0.2.4
    ./run.sh

### Windows:
    cd .\Tutorials\jena-fuseki-0.2.4
    run.bat

# 3. Start RStudio

# 4. Start Gephi

# 5. Open Tutorials
Go to the Tutorials section on linkedscience.org: http://linkedscience.org/tutorials/
or: Open the HTML files in this directory in a web browser

A *VERY* short introduction to R can be found in ./Tutorials/R_intro.R

A slightly longer tutorial to R and RStudio can be found in ./Setup/Torfs+Brauer-Short-R-Intro.pdf

A *VERY* short introduction to SPARQL can be found in ./Tutorials/SPARQL_intro.txt

A slightly longer tutorial to SPARQL can be found at http://jena.apache.org/tutorials/sparql.html


## Ubuntu R and RStudio installation

    # Make sure we're up to date
    sudo apt-get update
    sudo apt-get -y dist-upgrade

    # install the unofficial ppa source for R 2.15
    # (see https://stat.ethz.ch/pipermail/r-sig-debian/2012-June/001889.html)
    sudo apt-get -y install python-software-properties
    sudo add-apt-repository -y ppa:marutter/rrutter
    sudo apt-get update

    # install r  and needed libraries
    sudo apt-get install -y r-base xorg-dev freeglut3-dev libgdal1-dev libproj-dev g++ gfortran
    # Then run R, making sure the version number is 2.15
    $ R
    R version 2.15.1 (2012-06-22) -- "Roasted Marshmallows"
    [...]
    > 

    # You probably want to use RStudio, but it is not included in the default repositories yet
    # Download a deb from http://rstudio.org/download/desktop and use "sudo dkpg -i PACKAGE" to install it

    # In R (or RStudio), install the required packages:
    install.packages(c('SPARQL','sp','ggmap','mapproj','igraph','network','ergm','zoo','gsubfn','rjson'),dependencies=TRUE)


