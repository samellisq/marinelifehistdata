## Updates the Documentation (ie runs roxygen 2) Must be run before any changes to doumentation will take effect
#devtools::document()# this needs to be run in the console to update the docuemnation

##Adds the package to the imports section of the DESCRIPTION. Does not add any version information
#usethis::use_package("PACKAGENAME")
## special case for pipes:
# usethis::use_pipe(export = TRUE) # note also run use package magittr and document before it will work

## creates the ...-package.R file which can the be edited. Called when you ask ?packagename in the console
#devtools::use_package_doc()

##Create test directories and files
#usethis::use_testthat()

##runs the files in tests/testthat folder. NB all tests must start with tests.
#devtools::test()

##Create a Vignette folder and associated file with the name in the quotes. Gives a guide which can then be filled in
#devtools::use_vignette("Intro to nhsnumbergenerator")

##Load the package
#devtools::load_all()

##Install the package to use elsewhere
#.libPaths("C:/Users/se308/R Packages II") # route to packages needed to be used on the laptop
#devtools::install()

# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


##### GIT
# https://happygitwithr.com/existing-github-last.html
# check website but it should be as simple as
# 1. usethis::use_github()
# 2. Stage everything using the git panel
# 3. usethis::use_github()
# (note this all assumes that SSH is set up and all that)
