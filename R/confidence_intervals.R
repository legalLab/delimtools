confidence_intervals <- function(method, args) {
  
  gmyc_ci <- function(tr, posterior, method= "single", interval= c(0, 5)){
    
    # combine trees and remove names
    trees <- c(tr, posterior) |> unname()
    
    # get a quiet GMYC function
    gmyc_quietly <- purrr::quietly(splits::gmyc)
    
    # run gmyc over trees and combine results
    gmyc_res <- furrr::future_map(trees, 
                                  ~{gmyc_quietly(.x, method= method, interval= interval) |>
                                      purrr::pluck("result") |>
                                      delimtools::gmyc_tbl() |>
                                      dplyr::pull(2) |>
                                      vctrs::vec_unique_count()},
                                  .options= furrr::furrr_options(seed = TRUE)) |>
      purrr::list_c()
    
  }
  
  bgmyc_ci <- function(tr, posterior, ppcutoff= 0.05, mcmc, burnin, 
                       thinning, py1 = 0, py2 = 2, pc1 = 0, pc2 = 2, t1 = 2, 
                       t2 = 51, scale = c(20, 10, 5), start = c(1, 0.5, 50)) {
    
    # combine trees and remove names
    trees <- c(tr, posterior) |> unname()
    
    # get a quiet bGMYC function
    bgmyc_quietly <- purrr::quietly(bGMYC::bgmyc.singlephy)
    
    # run bGMYC over trees
    bgmyc_res <- furrr::future_map(trees,
                                   ~{bgmyc_quietly(.x,
                                                   mcmc= mcmc,
                                                   burnin= burnin,
                                                   thinning= thinning,
                                                   py1= py1,
                                                   py2= py2,
                                                   pc1= pc1,
                                                   pc2= pc2,
                                                   t1= t1,
                                                   t2= t2,
                                                   scale= scale,
                                                   start= start) |>
                                       purrr::pluck("result") |>
                                       delimtools::bgmyc_tbl(ppcutoff= ppcutoff) |>
                                       dplyr::pull(2) |>
                                       vctrs::vec_unique_count()},
                                   .options= furrr::furrr_options(seed = TRUE)) |>
      purrr::list_c()
    
  }
  
  locmin_ci <- function(fasta, block, reps, threshold= NULL, haps= NULL, ...){
    
    mat <- ape::dist.dna(fasta, ...)
    
    boot_dist <- purrr::map(seq(1, reps), ~delimtools:::boot_dna(fasta, block)) |>
      purrr::map(ape::dist.dna, ...)
    
    dist_ls <- append(boot_dist, list(mat), after=0)
    
    locmin_quietly <- purrr::quietly(spider::localMinima)
    
    if(is.null(threshold)){
      
      threshold <- locmin_quietly(mat) |>
        purrr::pluck("result", "localMinima", 1)
      
      cli::cli_warn("{cli::col_yellow({cli::symbol$warning})} Argument {.arg threshold} not provided. 
                    Using {.val {threshold}} as cutoff value for clustering.")
    }
    
    if(threshold == "multiple"){
      
      threshold_ls <- map(dist_ls, 
                          ~{locmin_quietly(.x) |>
                              purrr::pluck("result", "localMinima", 1)}) |>
        purrr::list_c()
      
      locmin_res <- purrr::map2(dist_ls, threshold_ls, 
                                ~{delimtools::locmin_tbl(.x, .y, haps) |>
                                    dplyr::pull(2) |>
                                    vctrs::vec_unique_count()}) |>
        purrr::list_c()
      
    } else {
      
      locmin_res <- purrr::map(dist_ls, 
                               ~{.x |> 
                                   delimtools::locmin_tbl(threshold, haps) |>
                                   dplyr::pull(2) |>
                                   vctrs::vec_unique_count()}) |>
        purrr::list_c()
      
    }
  }
  
  mptp_ci <- function(infile, bootstraps, exe = NULL, outfolder = NULL, 
                      method = c("multi", "single"),minbrlen = 0.0001, 
                      webserver = NULL) {
    
    tr_ml <- ape::read.tree(file= infile)
    
    if(!methods::is(bootstraps, "multiPhylo")){
      
      cli::cli_warn("{cli::col_yellow({cli::symbol$warning})} Argument {.arg bootstraps} 
                    must have class {.cls multiPhylo}. Try reading trees using {.fn ape::read.tree}")
      
    }
    
    trees <- c(tr_ml, bootstraps) |> unname()
    
    names(trees) <- c("atr", paste0("bootstrap_tree_",seq(1, length(bootstraps))))
    
    if(is.null(outfolder)){
      
      outfolder <- tempdir()
      
    }
    
    purrr::walk2(trees, names(trees), ~{ape::write.tree(.x, file= glue::glue("{outfolder}/{.y}.nwk"))})
    
    infiles <- list.files(outfolder, pattern= ".nwk", full.names = TRUE) |> 
      gtools::mixedsort() |>
      as.list()
    
    if(is.null(minbrlen)){
      
      minbrlen <- delimtools::min_brlen(tr_ml) |> purrr::pluck("dist", 1)
      
    }
    
    if(is.numeric(minbrlen)){
      
      mptp_res <- purrr::map(infiles, 
                             ~{delimtools::mptp_tbl(infile= .x,
                                                    exe= exe,
                                                    outfolder= outfolder,
                                                    method= method,
                                                    minbrlen= minbrlen,
                                                    webserver= webserver) |>
                                 dplyr::pull(2) |>
                                 vctrs::vec_unique_count()}) |>
        purrr::list_c()
      
    } else if(minbrlen == "multiple"){
      
      minbr_ls <- map(trees, 
                      ~{delimtools::min_brlen(.x, print= FALSE)} |>
                        purrr::pluck("dist", 1))
      
      mptp_res <-  purrr::map2(infiles, minbr_ls, 
                               ~{delimtools::mptp_tbl(infile= .x,
                                                      exe= exe,
                                                      outfolder= outfolder,
                                                      method= method,
                                                      minbrlen= .y,
                                                      webserver= webserver) |>
                                   dplyr::pull(2) |>
                                   vctrs::vec_unique_count()}) |>
        purrr::list_c()
    }
  }
  
  arg_names <- names(args)
  
  ci_res <- purrr::map2(.x= method, .y= args, .f= ~{rlang::exec(.x, !!!.y)})
  
  ci_df <- purrr::map2(arg_names, ci_res, 
                       ~{tibble::tibble(method= .x,
                                        point_estimate= .y[1],
                                        CI_95= as.integer(quantile(.y[-1], probs= c(0.025, 0.975))) |>
                                          stringr::str_flatten(collapse = "-"),
                                        CI_mean= as.integer(mean(.y[-1])),
                                        CI_median= as.integer(stats::median(.y[-1])))}) |>
    purrr::list_rbind()
  
  return(ci_df)
}
