## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... [181s] NOTE
Maintainer: 'Pedro Bittencourt <pedro.sennabittencourt@gmail.com>'

Suggests or Enhances not in mainstream repositories:
  bGMYC, splits
Availability using Additional_repositories specification:
  bGMYC    yes   https://pedrosenna.github.io/drat/
  splits   yes   https://r-forge.r-project.org/    

Found the following (possibly) invalid URLs:
  URL: https://bioinfo.mnhn.fr/abi/public/abgd/
    From: man/abgd_tbl.Rd
    Status: Error
    Message: Timeout was reached [bioinfo.mnhn.fr]:
      Failed to connect to bioinfo.mnhn.fr port 443 after 21063 ms: Could not connect to server
  URL: https://bioinfo.mnhn.fr/abi/public/abgd/abgdweb.html
    From: man/abgd_tbl.Rd
          inst/doc/install.html
    Status: Error
    Message: Timeout was reached [bioinfo.mnhn.fr]:
      Failed to connect to bioinfo.mnhn.fr port 443 after 42140 ms: Could not connect to server
  URL: https://bioinfo.mnhn.fr/abi/public/asap/
    From: man/asap_tbl.Rd
          inst/doc/install.html
    Status: Error
    Message: Timeout was reached [bioinfo.mnhn.fr]:
      Connection timed out after 60015 milliseconds
  URL: https://bioinfo.mnhn.fr/abi/public/asap/asapweb.html
    From: man/asap_tbl.Rd
    Status: Error
    Message: Timeout was reached [bioinfo.mnhn.fr]:
      Operation timed out after 60015 milliseconds with 0 bytes received
