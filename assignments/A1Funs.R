makeSampleNames <- function(id){
    require(tibble)
    require(magrittr)
    err <- paste(
        "Your ID doesn't appear to have been specified correctly.\n",
        "It should be of the form a1234567."
    )
    id <- as.character(id)[[1]]
    id <- stringr::str_remove(id, "^a")
    if (stringr::str_length(id) != 7) stop(err)
    id <- as.integer(id)
    if (is.na(id)) stop(err)
    
    dt <- c("20190427", "2018_12_03", "010918", "03_May_2018")
    sex <- list(
        c("M", "F"),
        c("Male", "Fem"),
        c("M", "M"),
        c("F", "F")
    )
    grp <- list(
        c("Mut", "WT"),
        c("Ctrl", "Treat"),
        c("P_P", "P_Q", "Q_Q"),
        c("3mth", "6mth")
    )
    postdoc <- c("Sadoon", "Monique", "Meghan", "Brian", "Ashlee", "Connor", 
                 "Kandyse", "Chabrie", "Nathan", "Chad")
    suffix <- c("fq", "fastq", "fq.gz", "fastq.gz", "bam")
    
    set.seed(id)
    # Sample the reps per group
    groups <- sample(grp)[[1]]
    n <- sample(3:6, length(groups))
    N <- sum(n)
    # Sample the sex
    df <- tibble(
        groups = rep(groups, times = n),
        sex = sample(sex, 1)[[1]] %>% sample(N, replace = TRUE),
        date = sample(dt, 1),
        postdoc = rep_len(sample(postdoc, sample(1:2, 1, prob = c(7, 3))), N),
        reads = sample(c("R1", "R2"), 1),
        sample = paste0("S", seq_len(N)),
        suffix = sample(suffix, 1)
    )
    # Set a typo in one of the sex entries, but make it rare
    sex2Lower <- sample(c(TRUE, FALSE), N, TRUE, c(0.05, 0.95))
    df$sex[sex2Lower] <- stringr::str_to_lower(df$sex[sex2Lower])
    nc <- ncol(df)
    colOrder <- sample(seq_len(nc - 1), nc - 1)
    pre <- apply(df[,colOrder], MARGIN = 1, FUN = paste, collapse = "_") 
    # Now the sample names and export to the global environment
    samples <- paste(pre, df$suffix, sep = ".")
    assign("sampleNames", samples, envir = .GlobalEnv)
    # Export the library sizes
    librarySizes <- tibble(
        sampleName = sampleNames,
        lib.size = rpois(N, 2.5e7) + rnorm(N, 0, 2e6)
    )
    assign("librarySizes", librarySizes, envir = .GlobalEnv)
    
}
