makeRT <- function(id){
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
    
    ###############################
    ## Generate Random qPCR data ##
    ###############################
    
    # Set the possible cell comparisons
    cellComparisons <- c(
        "Th", "Treg", 
        "MCF7", "BT549", 
        "HeLa", "LNCaP",
        "HEK293", "CD25Hi",
        "Jurkat", "CD34+",
        "Lesion", "Tissue",
        "PBMC", "CD4+",
        "NKT", "CD8+",
        "Naive", "Memory",
        "CD141+", "CD303+",
        "Liver", "Kidney"
    ) %>%
        matrix(ncol = 2, byrow = TRUE)
    hk <- c("ACTNB", "GAPDH", "RPL19")
    goi <- c(
        "IL2RA", "CTLA4", "SEPT9", "LRRC32", "PSEN2", "NFKB", "FOXO1",
        "SORL1"
    )
    
    # Generate the random qPCR data
    set.seed(id)
    n <- sample(4:6, 1)
    logFC <- rnorm(1)
    mn <- rpois(1, 22)
    rt <- tibble(
        cellType = cellComparisons %>%
            .[sample.int(nrow(.),1),] %>%
            rep(each = n),
        pair = rep(seq_len(n), times = 2),
        hk = rnorm(2*n, rpois(1, 12), 0.5),
        goi = c(
            rnorm(n, mn, 0.6),
            rnorm(n, mn + logFC, 0.6)
        )
    ) %>%
        set_names(
            stringr::str_replace(names(.), "hk", sample(hk, 1))
        ) %>%
        set_names(
            stringr::str_replace_all(names(.), "goi", sample(goi, 1))
        ) %>%
        tidyr::pivot_longer(
            any_of(c(hk, goi)), names_to = "gene", values_to = "Ct"
        ) %>%
        dplyr::mutate(
            type = dplyr::case_when(
                gene %in% hk ~ "Housekeeper Gene",
                gene %in% goi ~ "Study Gene"
            ) %>% as.factor(),
            gene = as.factor(gene),
            cellType = as.factor(cellType)
        )
    assign("qPCR", rt, envir = .GlobalEnv)
    
}
chooseGeneSet <- function(id){
    
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
    
    gs <- c(
        "GO_RNA_CATABOLIC_PROCESS", "GO_PROTEIN_TARGETING", "GO_ORGANIC_CYCLIC_COMPOUND_CATABOLIC_PROCESS", 
        "GO_ESTABLISHMENT_OF_PROTEIN_LOCALIZATION_TO_ORGANELLE", "GO_PEPTIDE_BIOSYNTHETIC_PROCESS", 
        "GO_CELLULAR_AMIDE_METABOLIC_PROCESS", "GO_AMIDE_BIOSYNTHETIC_PROCESS", 
        "GO_ORGANONITROGEN_COMPOUND_BIOSYNTHETIC_PROCESS", "GO_PROTEIN_LOCALIZATION_TO_MEMBRANE", 
        "GO_MITOCHONDRIAL_ENVELOPE", "GO_CHROMOSOME", "GO_RNA_METABOLIC_PROCESS", 
        "GO_POSITIVE_REGULATION_OF_GENE_EXPRESSION", "GO_MITOCHONDRIAL_PART", 
        "GO_ION_TRANSPORT", "GO_CELLULAR_MACROMOLECULE_CATABOLIC_PROCESS", 
        "GO_PROTEIN_LOCALIZATION_TO_ORGANELLE", "GO_NEGATIVE_REGULATION_OF_BIOSYNTHETIC_PROCESS", 
        "GO_NEGATIVE_REGULATION_OF_RNA_BIOSYNTHETIC_PROCESS", "GO_INTRACELLULAR_PROTEIN_TRANSPORT", 
        "GO_CHROMOSOME_ORGANIZATION", "GO_TRANSMEMBRANE_TRANSPORT", "GO_POSITIVE_REGULATION_OF_RNA_BIOSYNTHETIC_PROCESS", 
        "GO_MACROMOLECULE_CATABOLIC_PROCESS", "GO_MITOCHONDRION", "GO_POSITIVE_REGULATION_OF_TRANSCRIPTION_BY_RNA_POLYMERASE_II", 
        "GO_REGULATION_OF_PROTEIN_MODIFICATION_PROCESS", "GO_REGULATION_OF_CELL_DIFFERENTIATION", 
        "GO_REGULATION_OF_PHOSPHORUS_METABOLIC_PROCESS", "GO_RESPONSE_TO_DRUG", 
        "GO_POSITIVE_REGULATION_OF_BIOSYNTHETIC_PROCESS", "GO_CELLULAR_PROTEIN_CONTAINING_COMPLEX_ASSEMBLY", 
        "GO_RNA_BINDING", "GO_NUCLEOLUS", "GO_PROTEIN_PHOSPHORYLATION", 
        "GO_INTRACELLULAR_TRANSPORT", "GO_REGULATION_OF_INTRACELLULAR_SIGNAL_TRANSDUCTION", 
        "GO_POSITIVE_REGULATION_OF_PROTEIN_METABOLIC_PROCESS", "GO_POSITIVE_REGULATION_OF_CATALYTIC_ACTIVITY", 
        "GO_POSITIVE_REGULATION_OF_MOLECULAR_FUNCTION", "GO_PROTEIN_CONTAINING_COMPLEX_ASSEMBLY", 
        "GO_ENDOPLASMIC_RETICULUM", "GO_DEFENSE_RESPONSE", "GO_NUCLEOPLASM_PART", 
        "GO_SECRETION", "GO_CYTOSKELETAL_PART", "GO_APOPTOTIC_PROCESS", 
        "GO_RESPONSE_TO_OXYGEN_CONTAINING_COMPOUND", "GO_RESPONSE_TO_CYTOKINE", 
        "GO_CELLULAR_RESPONSE_TO_OXYGEN_CONTAINING_COMPOUND", "GO_PROTEIN_DIMERIZATION_ACTIVITY", 
        "GO_PROTEOLYSIS", "GO_REGULATION_OF_CELL_DEATH", "GO_REGULATION_OF_TRANSPORT", 
        "GO_HOMEOSTATIC_PROCESS", "GO_CELLULAR_MACROMOLECULE_LOCALIZATION", 
        "GO_REGULATION_OF_RESPONSE_TO_STRESS", "GO_IDENTICAL_PROTEIN_BINDING", 
        "GO_RIBONUCLEOTIDE_BINDING"
    )
    
    set.seed(id)
    sample(gs, 1)
    
}
