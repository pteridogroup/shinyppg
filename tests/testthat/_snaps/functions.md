# subsetting to taxonomic group works on single taxon

    Code
      abac_data
    Output
                  taxonID    scientificName    scientificNameAuthorship taxonRank
      1564 wfo-4000000007       Abacopteris                         Fée     genus
      2656 wfo-6500000526 Thelypteridoideae                    C.F.Reed subfamily
      2732 wfo-7000000609  Thelypteridaceae          Ching ex Pic.Serm.    family
      2814 wfo-9000000420      Polypodiales                        Link     order
      2884 wfo-4100003341    Polypodiopsida Cronquist, Takht. & W.Zimm.     class
      2887 wfo-4100003344      Polypodiidae Cronquist, Takht. & W.Zimm.  subclass
      3231 wfo-4100005210      Aspleniineae     H.Schneid. & C.J.Rothf.  suborder
           parentNameUsageID nomenclaturalStatus
      1564    wfo-6500000526               valid
      2656    wfo-7000000609               valid
      2732    wfo-4100005210               valid
      2814    wfo-4100003344               valid
      2884              <NA>               valid
      2887    wfo-4100003341               valid
      3231    wfo-9000000420               valid
                                   namePublishedIn taxonomicStatus
      1564   Mém. Foug., 5. Gen. Filic. 309 (1852)        accepted
      2656              Phytologia 17: 254 (1968).        accepted
      2732                Webbia 24(2): 709 (1970)        accepted
      2814 Hort. Berol. 2: 5. 1833. (Jul-Dec 1833)        accepted
      2884                 Taxon 15: 133. Apr 1966        accepted
      2887                     Taxon 15: 133(1966)        accepted
      3231        J. Syst. Evol. 54(6): 594 (2016)        accepted
           acceptedNameUsageID   modified acceptedNameUsage   parentNameUsage
      1564                <NA> 2025-04-28              <NA> Thelypteridoideae
      2656                <NA> 2024-06-04              <NA>  Thelypteridaceae
      2732                <NA> 2025-05-09              <NA>      Aspleniineae
      2814                <NA> 2024-12-16              <NA>      Polypodiidae
      2884                <NA> 2024-12-18              <NA>              <NA>
      2887                <NA> 2024-12-16              <NA>    Polypodiopsida
      3231                <NA> 2025-05-09              <NA>      Polypodiales

# subsetting to taxonomic group works on multiple taxa

    Code
      sub_data
    Output
                  taxonID    scientificName    scientificNameAuthorship taxonRank
      1564 wfo-4000000007       Abacopteris                         Fée     genus
      1565 wfo-4000000052       Abrodictyum                     C.Presl     genus
      1920 wfo-4000016646      Habrodictyon                       Bosch     genus
      2064 wfo-4000022709        Macroglena            (C.Presl) Copel.     genus
      2380 wfo-4000034984     Selenodesmium             (Prantl) Copel.     genus
      2656 wfo-6500000526 Thelypteridoideae                    C.F.Reed subfamily
      2657 wfo-6500000527   Trichomanoideae                     C.Presl subfamily
      2704 wfo-7000000290  Hymenophyllaceae                       Mart.    family
      2732 wfo-7000000609  Thelypteridaceae          Ching ex Pic.Serm.    family
      2797 wfo-9000000264   Hymenophyllales                   A.B.Frank     order
      2814 wfo-9000000420      Polypodiales                        Link     order
      2884 wfo-4100003341    Polypodiopsida Cronquist, Takht. & W.Zimm.     class
      2887 wfo-4100003344      Polypodiidae Cronquist, Takht. & W.Zimm.  subclass
      3082 wfo-4100004512      Pachychaetum                     C.Presl     genus
      3231 wfo-4100005210      Aspleniineae     H.Schneid. & C.J.Rothf.  suborder
           parentNameUsageID nomenclaturalStatus
      1564    wfo-6500000526               valid
      1565    wfo-6500000527               valid
      1920              <NA>                <NA>
      2064              <NA>                <NA>
      2380              <NA>                <NA>
      2656    wfo-7000000609               valid
      2657    wfo-7000000290               valid
      2704    wfo-9000000264               valid
      2732    wfo-4100005210               valid
      2797    wfo-4100003344               valid
      2814    wfo-4100003344               valid
      2884              <NA>               valid
      2887    wfo-4100003341               valid
      3082              <NA>                <NA>
      3231    wfo-9000000420               valid
                                               namePublishedIn taxonomicStatus
      1564               Mém. Foug., 5. Gen. Filic. 309 (1852)        accepted
      1565                  Hymenophyllaceae [Presl] 20 (1843)        accepted
      1920                       Hymenophyll. Javan. 17 (1861)         synonym
      2064               Philippine Journal of Science 67 1938         synonym
      2380                      Philipp. J. Sci. 67: 80 (1938)         synonym
      2656                          Phytologia 17: 254 (1968).        accepted
      2657                      Hymenophyllaceae: 102. (1843).        accepted
      2704                Consp. Regn. Veg. [Martius] 3 (1835)        accepted
      2732                            Webbia 24(2): 709 (1970)        accepted
      2797              Syn. Pflanzenk. (ed. 2) 3: 1453. 1877.        accepted
      2814             Hort. Berol. 2: 5. 1833. (Jul-Dec 1833)        accepted
      2884                             Taxon 15: 133. Apr 1966        accepted
      2887                                 Taxon 15: 133(1966)        accepted
      3082 Abh. Königl. Böhm. Ges. Wiss., ser. 5, 3: 16 (1843)         synonym
      3231                    J. Syst. Evol. 54(6): 594 (2016)        accepted
           acceptedNameUsageID   modified acceptedNameUsage   parentNameUsage
      1564                <NA> 2025-04-28              <NA> Thelypteridoideae
      1565                <NA> 2025-03-19              <NA>   Trichomanoideae
      1920      wfo-4000000052 2025-03-19       Abrodictyum              <NA>
      2064      wfo-4000000052 2024-12-16       Abrodictyum              <NA>
      2380      wfo-4000000052 2024-12-16       Abrodictyum              <NA>
      2656                <NA> 2024-06-04              <NA>  Thelypteridaceae
      2657                <NA> 2024-12-16              <NA>  Hymenophyllaceae
      2704                <NA> 2025-03-19              <NA>   Hymenophyllales
      2732                <NA> 2025-05-09              <NA>      Aspleniineae
      2797                <NA> 2024-12-16              <NA>      Polypodiidae
      2814                <NA> 2024-12-16              <NA>      Polypodiidae
      2884                <NA> 2024-12-18              <NA>              <NA>
      2887                <NA> 2024-12-16              <NA>    Polypodiopsida
      3082      wfo-4000000052 2024-12-16       Abrodictyum              <NA>
      3231                <NA> 2025-05-09              <NA>      Polypodiales

