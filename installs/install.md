# Installing Species Delimitation Softwares

`delimtools` has the legacy feature to process the output of
delimitations carried out on webservers (ABGD or ASAP) and to call the
original external implementations of these tools. To carry out
delimitation in the original implementations of the functions, it is
necessary to install third-party software and use the appropriate
`*_tbl()` functions to parse these results in a tabular format.

<table>
<thead>
<tr class="header">
<th style="text-align: left;">Function</th>
<th style="text-align: left;">Software</th>
<th style="text-align: center;">Language</th>
<th style="text-align: left;">Source</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">abgd_tbl()</td>
<td style="text-align: left;">ABGD</td>
<td style="text-align: center;">C</td>
<td style="text-align: left;"><a href="https://github.com/iTaxoTools/ABGDpy/tree/main/src/abgd" class="uri">https://github.com/iTaxoTools/ABGDpy/tree/main/src/abgd</a></td>
</tr>
<tr class="even">
<td style="text-align: left;">asap_tbl()</td>
<td style="text-align: left;">ASAP</td>
<td style="text-align: center;">C</td>
<td style="text-align: left;"><a href="https://github.com/iTaxoTools/ASAPy/tree/main/src/asap" class="uri">https://github.com/iTaxoTools/ASAPy/tree/main/src/asap</a></td>
</tr>
<tr class="odd">
<td style="text-align: left;">bgmyc_tbl()</td>
<td style="text-align: left;">bGMYC</td>
<td style="text-align: center;">R</td>
<td style="text-align: left;"><a href="https://nreid.github.io/software" class="uri">https://nreid.github.io/software</a></td>
</tr>
<tr class="even">
<td style="text-align: left;">gmyc_tbl()</td>
<td style="text-align: left;">splits</td>
<td style="text-align: center;">R</td>
<td style="text-align: left;"><a href="https://splits.r-forge.r-project.org" class="uri">https://splits.r-forge.r-project.org</a></td>
</tr>
<tr class="odd">
<td style="text-align: left;">locmin_tbl()</td>
<td style="text-align: left;">spider</td>
<td style="text-align: center;">R</td>
<td style="text-align: left;"><a href="https://github.com/boopsboops/spider" class="uri">https://github.com/boopsboops/spider</a></td>
</tr>
<tr class="even">
<td style="text-align: left;">mptp_tbl()</td>
<td style="text-align: left;">mptp</td>
<td style="text-align: center;">C</td>
<td style="text-align: left;"><a href="https://github.com/Pas-Kapli/mptp" class="uri">https://github.com/Pas-Kapli/mptp</a></td>
</tr>
</tbody>
</table>

Install R packages
------------------

### spider

Although some of the listed software above are written using R language,
only the `spider` package is available on CRAN, and its installation is
straightforward:

    install.packages("spider")

### splits

The `splits` package can be installed using R-Forge repository. Set
`dependencies = TRUE` to install `paran` package.

    install.packages("splits", repos = "https://r-forge.r-project.org/", dependencies = TRUE)

### bGMYC

Although the `bGMYC` package can be installed from Noah Reid’s GitHub
page, we recommend installing it from our [drat
repository](https://github.com/pedrosenna/drat) since our version is
stable when using `R >= 4.x.x`. When running the “older” `bGMYC` version
using `R >= 4.x.x`, all tips of the ultrametric tree are classified as
distinct lineages. Thus, install the “stable” version by using
`devtools` or by downloading the tarball and installing it manually:

    install.packages("bGMYC", repos = "https://pedrosenna.github.io/drat/")

Install C softwares
-------------------

The instructions below are to compile `ABGD`, `ASAP`, and `mPTP` on Unix
systems. If you are a Windows user, you can try to use a C compiler to
create executable files for these softwares, but we do not guarantee
they will work as intended. Alternatively, you may use the currently
available Web servers of these software to generate results and import
them using its respective `*_tbl()` functions.

### ABGD

To install `ABGD`, download latest release, decompress and compile.
Optionally, you may add the executable to your PATH environment variable
or move it to `/usr/local/bin`.

    mkdir ABGD
    cd ABGD
    # manually download the content of https://github.com/iTaxoTools/ABGDpy/tree/main/src/abgd
    make
    mkdir bin
    mv abgd bin/abgd
    cd ..

### ASAP

To install `ASAP`, download latest release, decompress and compile.
Optionally, you may add the executable to your PATH environment variable
or move it to `/usr/local/bin`.

    mkdir ASAP
    cd ASAP
    # manually download the content of https://github.com/iTaxoTools/ASAPy/tree/main/src/asap
    make
    mkdir bin
    mv asap bin/asap
    cd ..

### mPTP

To install `mPTP`, you can check [mPTP
repository](https://github.com/Pas-Kapli/mptp) and follow their
instructions.
