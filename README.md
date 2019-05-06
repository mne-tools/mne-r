
# MNE-R: fast acccess to MNE-Python from within R

<img src="man/figures/mne_logo.png" align="right" alt="" width="160" />

The [MNE-Python](https://mne-tools.github.io/stable/index.html) project
provides a full tool stack for processing and visualizing
electrophysiology data. That is, electroencephalography (EEG),
magnetoencephalography but also intracranial EEG.
[MNE-R](https://mne-tools.github.io/mne-r/index.html) facilitates
integrating this mature and extensive functionality into R-based data
processing, visualization and statisticasl modeling. This is made
possible through the [reticulate](https://rstudio.github.io/reticulate/)
package, which enables seamless integration of Python into R.

Currently, MNE-R is focussing on documenting how to use MNE-Python
through R, based on familiar
[MNE-examples](https://mne-tools.github.io/stable/auto_examples/index.html)
while also showcasing what R can add to the game in terms of statistics
and visualization functionality.

In the future, more R-functions may be added that facilitate the
interaction with MNE-Python or implement complementary functionality.

The project is open to contributions.

## Getting Started

In order to use MNE-R, MNE-Python has to be installed with all its
dependencies. Some configuration may be needed to make sure `reticulate`
knows which Python installation to use. Please consider the
[reticulate](https://rstudio.github.io/reticulate/articles/calling_python.html)
and the [MNE](https://mne-tools.github.io/stable/getting_started.html)
documentation. We generally recommend using the
[Anaconda](https://www.anaconda.com) Python distribution and Python 3
instead of Python 2.

For seamlessly combining R and Python code in one Rmarkdown script, the
latest
[Rstudio 1.2](https://blog.rstudio.com/2019/04/30/rstudio-1-2-release/)
is needed.

Currently, MNE-R can be installed from github.

``` r
library(devtools)
devtools::install_github("mne-tools/mne-r")
```

To get started, simply laod the mne library

``` r
library(mne)  # load mne and get the mne object
#> Importing MNE version=0.18.dev0, path='/Users/dengeman/github/mne-python/mne'

# the mne object wraps the loaded mne module inside Python
print(names(mne)[1:10])
#>  [1] "AcqParserFIF"               "add_reference_channels"    
#>  [3] "add_source_space_distances" "annotations"               
#>  [5] "Annotations"                "apply_forward"             
#>  [7] "apply_forward_raw"          "average_forward_solutions" 
#>  [9] "BaseEpochs"                 "baseline"

# use dollar signs to access MNE modules, functions and objects
cat(mne$datasets$sample$data_path$`__doc__`)
#> Get path to local copy of sample dataset.
#> 
#>     Parameters
#>     ----------
#>     path : None | str
#>         Location of where to look for the sample dataset.
#>         If None, the environment variable or config parameter
#>         ``MNE_DATASETS_SAMPLE_PATH`` is used. If it doesn't exist, the
#>         "~/mne_data" directory is used. If the sample dataset
#>         is not found under the given path, the data
#>         will be automatically downloaded to the specified folder.
#>     force_update : bool
#>         Force update of the sample dataset even if a local copy exists.
#>     update_path : bool | None
#>         If True, set the ``MNE_DATASETS_SAMPLE_PATH`` in mne-python
#>         config to the given path. If None, the user is prompted.
#>     download : bool
#>         If False and the sample dataset has not been downloaded yet,
#>         it will not be downloaded and the path will be returned as
#>         '' (empty string). This is mostly used for debugging purposes
#>         and can be safely ignored by most users.
#>     
#>     verbose : bool, str, int, or None
#>         If not None, override default verbose level (see :func:`mne.verbose`
#>         and :ref:`Logging documentation <tut_logging>` for more).
#> 
#>     Returns
#>     -------
#>     path : str
#>         Path to sample dataset directory.
```

## Troubleshooting and known issues.

  - If knitting Python chunks inside Rmarkdown fails, it is most likely
    a problem of telling `reticulate` which Python interpreter to use.
    To use the recommended Python 3 from Aanaconda, assuming it is
    installed, you can include this line in your `.bashrc` (Linux) or
    `.bash_profile` (Mac):

<!-- end list -->

``` bash
export RETICULATE_PYTHON=$HOME/anaconda3/bin/python
```

  - Also make sure your Python installation is ok. If you cannot run the
    Python code of interest using the Python interpreter, the problem is
    there. For exampple, make sure that inside the Python interpreter
    importing `mne` works.

<!-- end list -->

``` python
import mne
```

This should not give you any error message.

  - For optimal Python and `reticulate` support in Rstudio, make sure
    you have the latest version of
    [Rstudio](https://blog.rstudio.com/2019/04/30/rstudio-1-2-release/).

  - Currently, when making matplotlib figures from within R, the
    resulting image will not be rendered inside the Rstudio Rmarkdown
    chunk. You will need to save, load and display the figure or
    explicitly, write Python code in a Python chunk, or explicitly make
    maptlotlib open a window:

<!-- end list -->

``` r
library(mne)
plt <- import("matplotlib.pyplot")  # get matplotlib handle

data_path <- mne$datasets$sample$data_path()
raw_fname <- paste(data_path,
                   'MEG', 
                   'samlpe',
                   'sample_audvis_filt-0-40_raw.fif',
                   sep = '/')
raw <- mne$io$read_raw_fif(raw_fname)
raw$plot()  # plot it!
plt$show()  # show the figure, then search for the window popping up.
```

  - Some of the examples may depend on the latest MNE-Python code. We
    will try to provide patches little by little in MNE-Python or here.

  - Currently, if 32-bit and 64-bit versions of R are installed, it
    seems devtools tries to build for both of them. This yields a
    `loading failed for 'i386'` error. To force building the package for
    your currently running R version use the `INSTALL_opts` argument of
    `install_github`:

<!-- end list -->

``` r
devtools::install_github("mne-tools/mne-r", INSTALL_opts=c("--no-multiarch"))
```

## Roadmap

1.  Add many vignettes and examples
2.  Find common inconvenient steps and add convenience R functions
3.  Motivate contributed examples and code from the R-EEG community
4.  Add advanced content that leverages functionality unique to R
