# Installing Species Delimitation Softwares

## 

The main purpose of `delimtools` is to provide helper functions for
analyzing single locus species delimitation outputs. To generate this
type of data, it is necessary to install third-party software and use
the appropriate `*_tbl()` functions to parse these results in a tabular
format.

| Function     | Software | Language | Source                                       |
|:-------------|:---------|:--------:|:---------------------------------------------|
| abgd_tbl()   | ABGD     |    C     | bioinfo.mnhn.fr/abi/public/abgd/abgdweb.html |
| asap_tbl()   | ASAP     |    C     | bioinfo.mnhn.fr/abi/public/asap/             |
| bgmyc_tbl()  | bGMYC    |    R     | <https://nreid.github.io/software/>          |
| gmyc_tbl()   | splits   |    R     | <https://splits.r-forge.r-project.org/>      |
| locmin_tbl() | spider   |    R     | <https://github.com/boopsboops/spider>       |
| mptp_tbl()   | mptp     |    C     | <https://github.com/Pas-Kapli/mptp>          |

## Install R packages

### spider

Although some of the listed software above are written using R language,
only the `spider` package is available on CRAN, and its installation is
straightforward:

``` r

install.packages("spider")
```

### splits

The `splits` package can be installed using R-Forge repository. Set
`dependencies = TRUE` to install `paran` package.

``` r

install.packages("splits", repos = "https://r-forge.r-project.org/", dependencies = TRUE)
```

### bGMYC

Although the `bGMYC` package can be installed from Noah Reid’s GitHub
page, we recommend installing it from our [drat
repository](https://github.com/pedrosenna/drat) since our version is
stable when using `R >= 4.x.x`. When running the “older” `bGMYC` version
using `R >= 4.x.x`, all tips of the ultrametric tree are classified as
distinct lineages. Thus, install the “stable” version by using
`devtools` or by downloading the tarball and installing it manually:

``` r

install.packages("bGMYC", repos = "https://pedrosenna.github.io/drat/")
```

## Install C softwares

The instructions below are to compile `ABGD`, `ASAP`, and `mPTP` in Unix
systems. If you are a Windows user, you can try to use a C compiler to
create executable files for these software but we do not guarantee it
will work as intended. Alternativelly, you may use the currently
available Web servers of these software to generate results and import
them using its respective `*_tbl()` functions.

### ABGD

To install `ABGD`, download latest release, decompress and compile.
Optionally, you may add the folder to your PATH environment variable or
move it to `/usr/local/bin`.

``` bash
wget https\://bioinfo.mnhn.fr/abi/public/abgd/last.tgz -O abgd.tgz
tar -xzvf abgd.tgz
cd Abgd
make
mkdir bin
mv abgd bin/abgd
cd ..
```

### ASAP

To install `ASAP`, download latest release, decompress and compile.
Optionally, you may add the folder to your PATH environment variable or
move it to `/usr/local/bin`.

``` bash
wget https\://bioinfo.mnhn.fr/abi/public/asap/last.tgz
tar -xzvf last.tgz
cd ASAP
make
mkdir bin
mv asap bin/asap
cd ..
```

### mPTP

To install `mPTP`, you can check [mPTP
repository](https://github.com/Pas-Kapli/mptp) and follow their
instructions.
