# Supplementary Figure S1: PRISMA 2020 primary-analysis selection flow.
# Based on the original grid layout, with author-supplied revised exclusions.
# Run from this directory with Rscript Supplementary_Figure_S1_PRISMA2020_40sources.R.
if (.Platform$OS.type == "windows" && identical(Sys.getlocale("LC_CTYPE"), "C")) {
  suppressWarnings(Sys.setlocale("LC_CTYPE", "Chinese (Simplified)_China.936"))
}
suppressPackageStartupMessages(library(grid))
output_dir <- Sys.getenv("S1_OUTPUT_DIR", unset = ".")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
stem <- file.path(output_dir, "Supplementary_Figure_S1_PRISMA2020_40sources")

database_counts <- c(`Web of Science Core Collection`=3793L, Scopus=4381L, PubMed=626L)
identified_total <- sum(database_counts)
duplicates_removed <- 5446L
records_screened <- 3354L
records_excluded <- 2597L
reports_sought <- 757L
reports_not_retrieved <- 0L
reports_assessed <- 757L
exclusion_reasons <- c(
  `Outside the eligible population or review scope`=648L,
  `No appropriate comparator`=35L,
  `Insufficient extractable quantitative data`=29L,
  `Not original research`=9L
)
reports_excluded <- sum(exclusion_reasons)
database_reports_retained <- reports_assessed - reports_excluded
grey_reports_included <- 4L
primary_sources <- database_reports_retained + grey_reports_included
primary_comparisons <- 669L
broader_inventory_sources <- 45L
broader_inventory_comparisons <- 874L

stopifnot(
  identified_total == 8800L,
  identified_total - duplicates_removed == records_screened,
  records_screened - records_excluded == reports_sought,
  reports_sought - reports_not_retrieved == reports_assessed,
  reports_excluded == 721L,
  database_reports_retained == 36L,
  primary_sources == 40L,
  primary_comparisons == 669L,
  broader_inventory_sources == 45L,
  broader_inventory_comparisons - primary_comparisons == 205L
)

source_data <- data.frame(
  stage=c(rep("Identification",4),"Removed before screening",
          "Screening","Screening","Retrieval","Retrieval","Eligibility",
          rep("Full-text exclusions",length(exclusion_reasons)),
          "Full-text exclusions","Database-stream retained",
          "Additional grey literature","Primary meta-analysis","Primary meta-analysis",
          "Broader extraction inventory"),
  item=c(names(database_counts),"All databases","Duplicate records removed",
         "Unique records screened","Records excluded","Reports sought for retrieval",
         "Reports not retrieved","Reports assessed for eligibility",
         names(exclusion_reasons),"Reports excluded total",
         "Reports retained after full-text eligibility",
         "Included grey-literature sources outside saved database exports",
         "Unique sources","Treatment-control comparisons",
         "Unique sources in broader inventory"),
  n=c(unname(database_counts),identified_total,duplicates_removed,
      records_screened,records_excluded,reports_sought,
      reports_not_retrieved,reports_assessed,unname(exclusion_reasons),
      reports_excluded,database_reports_retained,grey_reports_included,
      primary_sources,primary_comparisons,broader_inventory_sources),
  provenance=c(rep("Saved database exports and revised Table S2",4),
               rep("Revised Table S2 / manuscript; row-level log unavailable",2),
               rep("Prior S1 flow; row-level screening log unavailable",4),
               rep("Author-supplied revised full-text exclusion count",4),
               "Sum of the four revised exclusion categories",
               "757 assessed minus 721 excluded",
               "Four included sources; Table S3 search-account status",
               rep("Final 20-outcome primary analysis input",2),
               "Revised manuscript extraction inventory"),
  stringsAsFactors=FALSE
)
write.csv(source_data,paste0(stem,"_source_data.csv"),row.names=FALSE,fileEncoding="UTF-8")

COL <- list(navy="#244A68",blue="#DCEAF3",blue_strong="#BFD7E6",
            grey="#F2F4F5",grey_mid="#D6DCE0",text="#1F2933",
            muted="#56616A",white="#FFFFFF")

draw_box <- function(x,y,w,h,lines,fill=COL$white,border=COL$navy,
                     fontsize=7.4,fontface="plain",line_step=1.30,
                     align="center",pad=0.018) {
  grid.roundrect(x=unit(x,"npc"),y=unit(y,"npc"),width=unit(w,"npc"),height=unit(h,"npc"),
                 r=unit(1.8,"mm"),gp=gpar(fill=fill,col=border,lwd=0.95))
  offsets <- ((length(lines)+1)/2-seq_along(lines))*fontsize*line_step
  x_text <- if(align=="left") x-w/2+pad else x
  just <- if(align=="left") c("left","center") else "center"
  for(i in seq_along(lines)) {
    grid.text(lines[i],x=unit(x_text,"npc"),y=unit(y,"npc")+unit(offsets[i],"pt"),
              just=just,gp=gpar(col=COL$text,fontsize=fontsize,
                                fontfamily="sans",fontface=fontface))
  }
}

draw_stage <- function(y,label,h) {
  grid.roundrect(x=unit(0.043,"npc"),y=unit(y,"npc"),width=unit(0.045,"npc"),
                 height=unit(h,"npc"),r=unit(1.5,"mm"),
                 gp=gpar(fill=COL$grey,col=COL$grey_mid,lwd=0.8))
  grid.text(label,x=unit(0.043,"npc"),y=unit(y,"npc"),rot=90,
            gp=gpar(col=COL$navy,fontsize=6.8,fontfamily="sans",fontface="bold"))
}

draw_arrow <- function(x0,y0,x1,y1) {
  grid.lines(x=unit(c(x0,x1),"npc"),y=unit(c(y0,y1),"npc"),
             arrow=arrow(type="closed",length=unit(1.8,"mm")),
             gp=gpar(col=COL$navy,lwd=0.95))
}

draw_flowchart <- function() {
  grid.newpage(); pushViewport(viewport(gp=gpar(fontfamily="sans")))
  grid.rect(gp=gpar(fill=COL$white,col=NA))
  grid.text(paste0("Supplementary Figure ", "S1"),x=unit(0.04,"npc"),y=unit(0.974,"npc"),
            just=c("left","top"),gp=gpar(col=COL$navy,fontsize=8.4,fontface="bold"))
  grid.text("PRISMA 2020 flow diagram of literature identification, screening and study inclusion",
            x=unit(0.04,"npc"),y=unit(0.941,"npc"),just=c("left","top"),
            gp=gpar(col=COL$text,fontsize=10.4,fontface="bold"))
  grid.text("Primary 20-outcome meta-analysis input",
            x=unit(0.04,"npc"),y=unit(0.912,"npc"),just=c("left","top"),
            gp=gpar(col=COL$muted,fontsize=7.0))

  draw_stage(0.823,"Identification",0.155)
  draw_stage(0.635,"Screening",0.180)
  draw_stage(0.465,"Eligibility",0.165)
  draw_stage(0.272,"Included",0.225)

  draw_box(0.305,0.823,0.405,0.145,c(
    "Records identified from databases (n = 8,800)",
    "Web of Science Core Collection (n = 3,793)",
    "Scopus (n = 4,381)","PubMed (n = 626)"),fill=COL$blue,fontsize=7.4)

  draw_box(0.760,0.823,0.365,0.145,c(
    "Records removed before screening:","Duplicate records removed (n = 5,446)",
    "(reported in Table S2 / manuscript)",
    "Row-level deduplication log unavailable"),
    fill=COL$grey,border=COL$grey_mid,fontsize=6.7,align="left",line_step=1.28)

  draw_box(0.305,0.690,0.405,0.070,c("Unique records screened","(n = 3,354)"),fontsize=7.7)
  draw_box(0.760,0.690,0.365,0.070,c("Records excluded","(n = 2,597)"),
           fill=COL$grey,border=COL$grey_mid,fontsize=7.5)
  draw_box(0.305,0.585,0.405,0.070,c("Reports sought for retrieval","(n = 757)"),fontsize=7.7)
  draw_box(0.760,0.585,0.365,0.070,c("Reports not retrieved","(n = 0)"),
           fill=COL$grey,border=COL$grey_mid,fontsize=7.5)
  draw_box(0.305,0.480,0.405,0.070,c("Reports assessed for eligibility","(n = 757)"),fontsize=7.7)
  draw_box(0.760,0.428,0.365,0.172,c(
    "Reports excluded (n = 721):",
    "Outside the eligible population or scope (n = 648)",
    "No appropriate comparator (n = 35)",
    "Insufficient extractable quantitative data (n = 29)",
    "Not original research (n = 9)"),
    fill=COL$grey,border=COL$grey_mid,fontsize=6.55,align="left",line_step=1.35)

  draw_box(0.305,0.350,0.405,0.070,c(
    "Reports retained from database stream",
    "after eligibility (n = 36)"),fontsize=7.5)
  draw_box(0.760,0.235,0.365,0.095,c(
    "Four included grey-literature sources (n = 4)",
    "3 labelled Google Scholar in Table S3;",
    "1 retrieval route unstated (P06)",
    "Not added to the 8,800 database records"),
    fill=COL$grey,border=COL$grey_mid,fontsize=6.35,align="left",line_step=1.27)
  draw_box(0.305,0.235,0.405,0.085,c(
    "20-outcome primary meta-analysis input",
    "40 sources; 669 comparisons"),
    fill=COL$blue_strong,fontsize=7.7,fontface="bold")

  draw_arrow(0.508,0.823,0.565,0.823)
  draw_arrow(0.305,0.750,0.305,0.725)
  draw_arrow(0.508,0.690,0.565,0.690)
  draw_arrow(0.305,0.655,0.305,0.620)
  draw_arrow(0.508,0.585,0.565,0.585)
  draw_arrow(0.305,0.550,0.305,0.515)
  draw_arrow(0.508,0.480,0.565,0.480)
  draw_arrow(0.305,0.445,0.305,0.385)
  draw_arrow(0.305,0.315,0.305,0.278)
  draw_arrow(0.578,0.235,0.508,0.235)

  grid.text(
    "Broader extraction inventory: 45 sources and 874 comparisons; this figure ends at the primary analysis input.",
    x=unit(0.04,"npc"),y=unit(0.112,"npc"),just=c("left","center"),
    gp=gpar(col=COL$muted,fontsize=6.0,fontfamily="sans"))
  grid.text(
    "Counts supplied for this revision: 757 - 721 = 36; 36 + 4 = 40. The four grey-literature routes lack search logs.",
    x=unit(0.04,"npc"),y=unit(0.089,"npc"),just=c("left","center"),
    gp=gpar(col=COL$muted,fontsize=6.0,fontfamily="sans"))
  grid.text(
    "Full-text exclusion changes from prior S1: comparator 34 to 35; insufficient data 22 to 29 (author-supplied).",
    x=unit(0.04,"npc"),y=unit(0.066,"npc"),just=c("left","center"),
    gp=gpar(col=COL$muted,fontsize=6.0,fontfamily="sans"))
  popViewport()
}

width_mm <- 178
height_mm <- 230
width_in <- width_mm/25.4
height_in <- height_mm/25.4

grDevices::cairo_pdf(paste0(stem,".pdf"),width=width_in,height=height_in,
                     family="Arial",bg="white")
draw_flowchart(); dev.off()
grDevices::png(paste0(stem,".png"),width=width_in,height=height_in,
               units="in",res=600,type="cairo",bg="white")
draw_flowchart(); dev.off()

legend_text <- c(
  paste0("Supplementary Figure ", "S1", " | ", "PRISMA 2020 flow diagram of literature identification, ",
         "screening and study inclusion."),
  paste0("Searches of the Web of Science Core Collection (n = 3,793), Scopus (n = 4,381) ",
         "and PubMed (n = 626) identified 8,800 records."),
  paste0("After 5,446 duplicate records were removed as reported in the manuscript, ",
         "3,354 unique records underwent title and ",
         "abstract screening; 2,597 records were excluded."),
  paste0("All 757 reports sought for retrieval were obtained and assessed for eligibility. ",
         "Of these, 721 were excluded because they were outside the eligible population or ",
         "review scope (n = 648), lacked an appropriate comparator (n = 35), lacked sufficient ",
         "extractable quantitative data (n = 29), or were not original research (n = 9)."),
  paste0("Thirty-six reports remained from this database stream. Four included grey-literature ",
         "sources were added outside the saved database exports, giving 40 sources and 669 ",
         "comparisons in the 20-outcome primary meta-analysis input."),
  paste0("The broader evidence inventory contains 45 sources and 874 comparisons. Table S3 ",
         "labels three of the four grey-literature sources as Google Scholar and leaves one ",
         "retrieval route unstated; no record-level search log was supplied for verification. ",
         "The revised full-text exclusion counts were provided by the author for this figure.")
)
writeLines(legend_text,paste0(stem,"_legend.txt"),useBytes=TRUE)
message("Created: ",normalizePath(output_dir,winslash="/"))
