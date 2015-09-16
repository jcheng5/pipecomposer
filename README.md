# Pipe Composer

Pipe Composer makes it easier to create [magrittr](https://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html) pipelines of dplyr/ggvis commands. As you write each stage of your pipeline, you can see the input (i.e. the output of the previous stage) to your left, and the output of the current stage to the right.

[Silent one-minute demo video](https://www.youtube.com/watch?v=rEA2G1aXLqw)

## Install

```r
devtools::install_github("jcheng5/pipecomposer")
```

## How to use

```r
library(pipecomposer)
pipecomposer()
```

## Usage

##### Navigation

Use the tab key to move the cursor between stages. Hover your mouse cursor over any stage to change the input/output panes to reflect that stage.


##### Retrieving your code

When you're done, close the browser tab or window and the final version of your code will be printed at the console. Or click the Print button to record intermediate versions of your code to the console.

##### Persistence

Stage values persist between runs of Pipe Composer during a single R session. So if you close your Pipe Composer window and then realize you need to make a change, simply calling `pipecomposer()` again will let you pick up where you left off.

##### Import code from clipboard

If you have an existing pipeline expression that you would like to import into Pipe Composer, copy it to the clipboard and run the `import_clipboard()` function. It will attempt to read the code from the clipboard, parse it into pipeline stages, and launch Pipe Composer.
