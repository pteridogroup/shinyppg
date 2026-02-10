# subsetting to taxonomic group works on single taxon

    Code
      abac_data
    Output
                  taxonID    scientificName    scientificNameAuthorship taxonRank
      1564 wfo-4000000007       Abacopteris                         Fée     genus
      2655 wfo-6500000526 Thelypteridoideae                    C.F.Reed subfamily
      2731 wfo-7000000609  Thelypteridaceae          Ching ex Pic.Serm.    family
      2813 wfo-9000000420      Polypodiales                        Link     order
      2883 wfo-4100003341    Polypodiopsida Cronquist, Takht. & W.Zimm.     class
      2886 wfo-4100003344      Polypodiidae Cronquist, Takht. & W.Zimm.  subclass
      3230 wfo-4100005210      Aspleniineae     H.Schneid. & C.J.Rothf.  suborder
           parentNameUsageID nomenclaturalStatus
      1564    wfo-6500000526               valid
      2655    wfo-7000000609               valid
      2731    wfo-4100005210               valid
      2813    wfo-4100003344               valid
      2883              <NA>               valid
      2886    wfo-4100003341               valid
      3230    wfo-9000000420               valid
                                   namePublishedIn taxonomicStatus
      1564   Mém. Foug., 5. Gen. Filic. 309 (1852)        accepted
      2655              Phytologia 17: 254 (1968).        accepted
      2731                Webbia 24(2): 709 (1970)        accepted
      2813 Hort. Berol. 2: 5. 1833. (Jul-Dec 1833)        accepted
      2883                 Taxon 15: 133. Apr 1966        accepted
      2886                     Taxon 15: 133(1966)        accepted
      3230        J. Syst. Evol. 54(6): 594 (2016)        accepted
           acceptedNameUsageID   modified acceptedNameUsage   parentNameUsage
      1564                <NA> 2025-04-28              <NA> Thelypteridoideae
      2655                <NA> 2024-06-04              <NA>  Thelypteridaceae
      2731                <NA> 2025-05-09              <NA>      Aspleniineae
      2813                <NA> 2024-12-16              <NA>      Polypodiidae
      2883                <NA> 2024-12-18              <NA>              <NA>
      2886                <NA> 2024-12-16              <NA>    Polypodiopsida
      3230                <NA> 2025-05-09              <NA>      Polypodiales
                    class     subclass        order     suborder           family
      1564 Polypodiopsida Polypodiidae Polypodiales Aspleniineae Thelypteridaceae
      2655 Polypodiopsida Polypodiidae Polypodiales Aspleniineae Thelypteridaceae
      2731 Polypodiopsida Polypodiidae Polypodiales Aspleniineae Thelypteridaceae
      2813 Polypodiopsida Polypodiidae Polypodiales         <NA>             <NA>
      2883 Polypodiopsida         <NA>         <NA>         <NA>             <NA>
      2886 Polypodiopsida Polypodiidae         <NA>         <NA>             <NA>
      3230 Polypodiopsida Polypodiidae Polypodiales Aspleniineae             <NA>
                   subfamily       genus
      1564 Thelypteridoideae Abacopteris
      2655 Thelypteridoideae        <NA>
      2731              <NA>        <NA>
      2813              <NA>        <NA>
      2883              <NA>        <NA>
      2886              <NA>        <NA>
      3230              <NA>        <NA>

# subsetting to taxonomic group works on multiple taxa

    Code
      sub_data
    Output
                  taxonID    scientificName    scientificNameAuthorship taxonRank
      1564 wfo-4000000007       Abacopteris                         Fée     genus
      1565 wfo-4000000052       Abrodictyum                     C.Presl     genus
      1920 wfo-4000016646      Habrodictyon                       Bosch     genus
      2064 wfo-4000022709        Macroglena            (C.Presl) Copel.     genus
      2379 wfo-4000034984     Selenodesmium             (Prantl) Copel.     genus
      2655 wfo-6500000526 Thelypteridoideae                    C.F.Reed subfamily
      2656 wfo-6500000527   Trichomanoideae                     C.Presl subfamily
      2703 wfo-7000000290  Hymenophyllaceae                       Mart.    family
      2731 wfo-7000000609  Thelypteridaceae          Ching ex Pic.Serm.    family
      2796 wfo-9000000264   Hymenophyllales                   A.B.Frank     order
      2813 wfo-9000000420      Polypodiales                        Link     order
      2883 wfo-4100003341    Polypodiopsida Cronquist, Takht. & W.Zimm.     class
      2886 wfo-4100003344      Polypodiidae Cronquist, Takht. & W.Zimm.  subclass
      3081 wfo-4100004512      Pachychaetum                     C.Presl     genus
      3230 wfo-4100005210      Aspleniineae     H.Schneid. & C.J.Rothf.  suborder
           parentNameUsageID nomenclaturalStatus
      1564    wfo-6500000526               valid
      1565    wfo-6500000527               valid
      1920              <NA>                <NA>
      2064              <NA>                <NA>
      2379              <NA>                <NA>
      2655    wfo-7000000609               valid
      2656    wfo-7000000290               valid
      2703    wfo-9000000264               valid
      2731    wfo-4100005210               valid
      2796    wfo-4100003344               valid
      2813    wfo-4100003344               valid
      2883              <NA>               valid
      2886    wfo-4100003341               valid
      3081              <NA>                <NA>
      3230    wfo-9000000420               valid
                                               namePublishedIn taxonomicStatus
      1564               Mém. Foug., 5. Gen. Filic. 309 (1852)        accepted
      1565                  Hymenophyllaceae [Presl] 20 (1843)        accepted
      1920                       Hymenophyll. Javan. 17 (1861)         synonym
      2064               Philippine Journal of Science 67 1938         synonym
      2379                      Philipp. J. Sci. 67: 80 (1938)         synonym
      2655                          Phytologia 17: 254 (1968).        accepted
      2656                      Hymenophyllaceae: 102. (1843).        accepted
      2703                Consp. Regn. Veg. [Martius] 3 (1835)        accepted
      2731                            Webbia 24(2): 709 (1970)        accepted
      2796              Syn. Pflanzenk. (ed. 2) 3: 1453. 1877.        accepted
      2813             Hort. Berol. 2: 5. 1833. (Jul-Dec 1833)        accepted
      2883                             Taxon 15: 133. Apr 1966        accepted
      2886                                 Taxon 15: 133(1966)        accepted
      3081 Abh. Königl. Böhm. Ges. Wiss., ser. 5, 3: 16 (1843)         synonym
      3230                    J. Syst. Evol. 54(6): 594 (2016)        accepted
           acceptedNameUsageID   modified acceptedNameUsage   parentNameUsage
      1564                <NA> 2025-04-28              <NA> Thelypteridoideae
      1565                <NA> 2025-03-19              <NA>   Trichomanoideae
      1920      wfo-4000000052 2025-03-19       Abrodictyum              <NA>
      2064      wfo-4000000052 2024-12-16       Abrodictyum              <NA>
      2379      wfo-4000000052 2024-12-16       Abrodictyum              <NA>
      2655                <NA> 2024-06-04              <NA>  Thelypteridaceae
      2656                <NA> 2024-12-16              <NA>  Hymenophyllaceae
      2703                <NA> 2025-03-19              <NA>   Hymenophyllales
      2731                <NA> 2025-05-09              <NA>      Aspleniineae
      2796                <NA> 2024-12-16              <NA>      Polypodiidae
      2813                <NA> 2024-12-16              <NA>      Polypodiidae
      2883                <NA> 2024-12-18              <NA>              <NA>
      2886                <NA> 2024-12-16              <NA>    Polypodiopsida
      3081      wfo-4000000052 2024-12-16       Abrodictyum              <NA>
      3230                <NA> 2025-05-09              <NA>      Polypodiales
                    class     subclass           order     suborder           family
      1564 Polypodiopsida Polypodiidae    Polypodiales Aspleniineae Thelypteridaceae
      1565 Polypodiopsida Polypodiidae Hymenophyllales         <NA> Hymenophyllaceae
      1920 Polypodiopsida Polypodiidae Hymenophyllales         <NA> Hymenophyllaceae
      2064 Polypodiopsida Polypodiidae Hymenophyllales         <NA> Hymenophyllaceae
      2379 Polypodiopsida Polypodiidae Hymenophyllales         <NA> Hymenophyllaceae
      2655 Polypodiopsida Polypodiidae    Polypodiales Aspleniineae Thelypteridaceae
      2656 Polypodiopsida Polypodiidae Hymenophyllales         <NA> Hymenophyllaceae
      2703 Polypodiopsida Polypodiidae Hymenophyllales         <NA> Hymenophyllaceae
      2731 Polypodiopsida Polypodiidae    Polypodiales Aspleniineae Thelypteridaceae
      2796 Polypodiopsida Polypodiidae Hymenophyllales         <NA>             <NA>
      2813 Polypodiopsida Polypodiidae    Polypodiales         <NA>             <NA>
      2883 Polypodiopsida         <NA>            <NA>         <NA>             <NA>
      2886 Polypodiopsida Polypodiidae            <NA>         <NA>             <NA>
      3081 Polypodiopsida Polypodiidae Hymenophyllales         <NA> Hymenophyllaceae
      3230 Polypodiopsida Polypodiidae    Polypodiales Aspleniineae             <NA>
                   subfamily       genus
      1564 Thelypteridoideae Abacopteris
      1565   Trichomanoideae Abrodictyum
      1920   Trichomanoideae Abrodictyum
      2064   Trichomanoideae Abrodictyum
      2379   Trichomanoideae Abrodictyum
      2655 Thelypteridoideae        <NA>
      2656   Trichomanoideae        <NA>
      2703              <NA>        <NA>
      2731              <NA>        <NA>
      2796              <NA>        <NA>
      2813              <NA>        <NA>
      2883              <NA>        <NA>
      2886              <NA>        <NA>
      3081   Trichomanoideae Abrodictyum
      3230              <NA>        <NA>

