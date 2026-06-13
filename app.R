# ==============================================================================
# SHINY APPLICATION - HEMOCM (DOOM FPS VIEW)
# ==============================================================================
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

# --- 1. PATH CONFIGURATION FOR SHINYAPPS.IO ---
DOSSIER_IMAGES <- "www"

addResourcePath("assets", DOSSIER_IMAGES)

HIST_FILE <- "historique_joueur.csv"

# --- 2. AUTOMATIC FILE SCANNER ---
fichiers_presents <- list.files(DOSSIER_IMAGES)

weak_f <- fichiers_presents[grepl("^weak_", fichiers_presents, ignore.case = TRUE)]
med_f <- fichiers_presents[grepl("^med_", fichiers_presents, ignore.case = TRUE)]
strong_f <- fichiers_presents[grepl("^strong_", fichiers_presents, ignore.case = TRUE)]
mboss_f <- fichiers_presents[grepl("^mboss_", fichiers_presents, ignore.case = TRUE)]
fboss_f <- fichiers_presents[grepl("^fboss_", fichiers_presents, ignore.case = TRUE)]
gun_f <- fichiers_presents[grepl("^gun_", fichiers_presents, ignore.case = TRUE)]

pioche_monstre <- function(liste_fichiers) {
  if (length(liste_fichiers) == 0) return("")
  return(paste0("assets/", sample(liste_fichiers, 1)))
}

trouve_arme <- function(num) {
  f <- gun_f[grepl(paste0("^gun_", num), gun_f)]
  if (length(f) > 0) return(paste0("assets/", f[1]))
  return("")
}

safe_sample <- function(x, size, replace = FALSE) {
  if (length(x) == 0) return(rep("None", size))
  if (length(x) == 1) {
    if (size == 1) return(x)
    return(rep(x, size))
  } else {
    if (!replace && length(x) < size) { replace <- TRUE }
    return(x[sample.int(length(x), size, replace = replace)])
  }
}

# ------------------------------------------------------------------------------
# 3. EXPERT KNOWLEDGE BASE (COMPREHENSIVE)
# ------------------------------------------------------------------------------
pathologies <- list(
  # --- MYELOPROLIFERATIVE NEOPLASMS (MPN) ---
  "Chronic Myeloid Leukemia (CML)" = list(cyto="t(9;22)(q34;q11)", mut="BCR::ABL1 fusion transcript (p210)", cd="CD13+, CD33+, CD11b+, CD15+, CD117+, CD34+ (on blasts)", clin="Harmonious left shift (myelemia), constant basophilia, massive splenomegaly", morpho="Global hypercellularity, dwarf hypolobated megakaryocytes, pseudo-Gaucher cells", trait="TKIs (Imatinib, Nilotinib, Dasatinib)"),
  "Polycythemia Vera (PV)" = list(cyto="Trisomy 8, trisomy 9, del(20q)", mut="JAK2 V617F (exon 14) or JAK2 exon 12", cd="Dominant erythroid lineage (CD71+, CD36+)", clin="Aquagenic pruritus, erythromelalgia, panmyelosis, thrombotic risk", morpho="Global erythroid hyperplasia, pleomorphic megakaryocytes forming loose clusters", trait="Phlebotomy, Low-dose aspirin, Hydroxyurea, Ruxolitinib"),
  "Essential Thrombocythemia (ET)" = list(cyto="Mostly normal karyotype", mut="JAK2 V617F, CALR (exon 9), or MPL (W515)", cd="Dominant megakaryocytic lineage (CD41+, CD61+, CD42b+)", clin="Isolated thrombocytosis > 450 G/L, hemorrhagic risk if platelets > 1500 G/L", morpho="Giant, hyperlobated megakaryocytes with 'stag-horn' nuclei", trait="Hydroxyurea, Anagrelide, Aspirin"),
  "Primary Myelofibrosis (PMF)" = list(cyto="del(20q), +8, del(13q)", mut="JAK2 V617F, CALR, MPL, and high-risk mutations (ASXL1, EZH2, SRSF2, IDH1/2)", cd="Circulating CD34+ progenitors", clin="Leukoerythroblastosis, massive splenomegaly, night sweats, weight loss", morpho="Dacryocytes (tear-drop cells), dystrophic micromegakaryocytes, dense reticulin fibrosis", trait="Ruxolitinib, Fedratinib, Allogeneic HSCT"),
  "Chronic Neutrophilic Leukemia (CNL)" = list(cyto="Normal karyotype", mut="CSF3R (T618I), ASXL1", cd="CD13+, CD33+, CD15+, CD16+, CD11b+, CD10+", clin="Isolated neutrophilic leukocytosis > 25 G/L, hepatosplenomegaly", morpho="Mature neutrophils with toxic granulations, lack of significant myeloblast expansion", trait="Ruxolitinib, Hydroxyurea"),
  "Chronic Eosinophilic Leukemia (CEL)" = list(cyto="Cryptic del(4q12) or t(1;4) or t(5;12)", mut="Fusion genes FIP1L1::PDGFRA, PDGFRB, FGFR1, JAK2", cd="CD13+, CD33+, CD15+, CD9+, CD25+", clin="Löffler endocarditis, endomyocardial fibrosis, pneumopathy", morpho="Hypereosinophilia with degranulation anomalies, vacuolated eosinophils", trait="Ultra-low dose Imatinib (if PDGFRA/B)"),
  
  # --- MASTOCYTOSIS ---
  "Systemic Mastocytosis (SM)" = list(cyto="Non-specific", mut="KIT D816V (codon 816), JAK2, ETV6, U2AF1, EZH2, SF3B1", cd="Strong CD117+, FcεRI+, CD25+, CD2+, CD30+", clin="Pruritus, headaches, diarrhea, anaphylactic shock, tryptase > 20 ng/mL", morpho="Dense multifocal infiltrates (>15 aggregated mast cells), atypical type I mast cells", trait="Midostaurin, Avapritinib"),
  "Aggressive Systemic Mastocytosis (ASM)" = list(cyto="Non-specific", mut="KIT D816V, TET2, N/KRAS, IDH2, SRSF2, ASXL1, RUNX1", cd="CD2- CD25- (or CD25+/- if SM-AHN)", clin="Organ damage (C-findings), often associated with an associated hematologic neoplasm (SM-AHN)", morpho="Atypical type II mast cells (immature bilobed nuclei, hypogranular cytoplasm)", trait="Midostaurin, Avapritinib, Azacitidine"),
  "Well-Differentiated Systemic Mastocytosis (WDSM)" = list(cyto="Non-specific", mut="Absence of KIT codon 816 mutation", cd="CD25- and CD2-, CD30+", clin="Systemic involvement lacking the classic mutation", morpho="Bone marrow infiltration by morphologically mature (round and heavily granulated) mast cells", trait="Non-specific"),
  
  # --- MYELODYSPLASTIC SYNDROMES (MDS) / CMML / MIXED ---
  "Chronic Myelomonocytic Leukemia (CMML)" = list(cyto="Monosomy 7, trisomy 8, loss of Y", mut="TET2, SRSF2, ASXL1, SETBP1, NRAS/KRAS", cd="Classical monocytes CD14+ CD16- (> 94%), aberrant CD56+", clin="Absolute monocytosis > 0.5 G/L and > 10% of leukocytes, autoimmunity", morpho="Dystrophic monocytes (folded nuclei), pseudo-Pelger-Huët anomaly in neutrophils", trait="Azacitidine, Venetoclax, Hydroxyurea"),
  "MDS/MPN with Ring Sideroblasts and Thrombocytosis" = list(cyto="Non-specific", mut="SF3B1 (essential), JAK2, ASXL1, TET2, DNMT3A, SETBP1", cd="Non-specific", clin="Sideroblastic anemia, Thrombocytosis", morpho="Ring sideroblasts", trait="Non-specific"),
  "MDS/MPN with Neutrophilia" = list(cyto="+13, del(12)p, del(20)q, i(17q)", mut="SETBP1, ETNK1, ASXL1, TET2, DNMT3A", cd="Non-specific", clin="Hepatosplenomegaly, Leukocytosis >= 13 G/L, Immature myeloid cells >= 10%", morpho="Multilineage dysplasia, Basophils < 2%, Monocytes < 10%", trait="Non-specific"),
  "MDS with isolated del(5q)" = list(cyto="Isolated del(5q) or associated with one other anomaly (excluding -7/del(7q))", mut="TP53 (mandatory prognostic screening)", cd="Loss of entropy in CD34+ CD38+ progenitors", clin="1 or 2 cytopenias, Absence of thrombocytopenia, female predominance", morpho="Dysplasia in 1-3 lineages, small hypolobated or unilobated (dwarf) megakaryocytes", trait="Lenalidomide"),
  "MDS with mutated SF3B1" = list(cyto="Absence of del(5q), -7/del(7q), or complex karyotype", mut="SF3B1 mutation (founder splicing mutation)", cd="Loss of entropy in CD34+ CD38+ progenitors", clin="Iron overload, isolated aregenerative macrocytic anemia", morpho=">= 15% ring sideroblasts (or >= 5% if SF3B1 mutated) via Perls stain", trait="Luspatercept, EPO, Hypomethylating agents"),
  "MDS with bi-allelic TP53 inactivation" = list(cyto="del(17p) + 1 TP53 mutation", mut=">= 2 TP53 mutations", cd="Loss of entropy in CD34+ CD38+ progenitors, MPP profile", clin="Severe pancytopenia, highly adverse prognosis", morpho="< 20% blasts in marrow and blood", trait="Azacitidine, Allogeneic HSCT"),
  "MDS with Excess Blasts (MDS-EB1 and MDS-EB2)" = list(cyto="Loss of Y, del(11q), del(12p), del(20q), del(7q), i(17q), trisomy 8, trisomy 19, Monosomy 7", mut="ASXL1, RUNX1, EZH2, DNMT3A, U2AF1, ZRSR2", cd="Loss of entropy in CD34+ CD38+ progenitors", clin="Anemic, hemorrhagic, infectious syndromes; Sweet syndrome, Relapsing polychondritis", morpho="MDS-EB1: 5-9% marrow blasts / MDS-EB2: 10-19% marrow blasts or Auer rods", trait="Azacitidine, Allogeneic HSCT, Transfusions"),
  
  # --- ACUTE MYELOID LEUKEMIAS (AML) ---
  "AML M0 (Minimally Differentiated)" = list(cyto="Non-specific", mut="Non-specific", cd=">= 2 myeloid markers (CD13, CD33, CD117), possible CD7+", clin="Non-specific", morpho="Myeloblasts without granulations or Auer rods (MPO < 3%)", trait="Standard 3+7 induction"),
  "AML M1 (Without Maturation)" = list(cyto="Non-specific", mut="Non-specific", cd=">= 2 myeloid markers (CD13, CD33, CD117)", clin="Non-specific", morpho="Myeloblasts with granulations, rare Auer rods (MPO >= 3%), granulocytic maturation < 10%", trait="Standard 3+7 induction"),
  "AML M2 (With Maturation)" = list(cyto="Non-specific", mut="Non-specific", cd=">= 2 myeloid markers (CD13, CD33, CD117)", clin="Non-specific", morpho="Granulocytic maturation >= 10% (promyelocytes, myelocytes, metamyelocytes, neutrophils)", trait="Standard 3+7 induction"),
  "AML M3 (Acute Promyelocytic Leukemia)" = list(cyto="t(15;17)(q24;q21)", mut="PML::RARA, FLT3-ITD and WT1 co-mutations", cd="Strong CD33, CD117+, CD9+, HLA-DR negative, CD34 negative, CD11b negative", clin="Absolute medical emergency, severe DIC, acute fibrinolysis", morpho="Blasts with innumerable granulations (faggot cells), bilobed nuclei", trait="ATRA (All-Trans Retinoic Acid) + Arsenic Trioxide (ATO)"),
  "AML M4 (Acute Myelomonocytic Leukemia)" = list(cyto="Non-specific", mut="Non-specific", cd="Non-specific", clin="Non-specific", morpho=">= 20% granulocytic and >= 20% monocytic differentiation", trait="Standard 3+7 induction"),
  "AML M5 (Acute Monoblastic Leukemia)" = list(cyto="Non-specific", mut="Non-specific", cd=">= 2 monocytic markers (CD11c, CD14, CD36, CD64)", clin="Non-specific", morpho="Monoblasts + promonocytes + monocytes > 80% in the marrow", trait="Standard 3+7 induction"),
  "AML M6 (Pure Erythroid Leukemia)" = list(cyto="Non-specific", mut="Non-specific", cd="Non-specific", clin="Non-specific", morpho="Erythroid precursors > 80% in marrow (>30% proerythroblasts), cytoplasmic vacuoles and blebs", trait="Standard 3+7 induction"),
  "AML M7 (Acute Megakaryoblastic Leukemia)" = list(cyto="Non-specific", mut="Non-specific", cd="CD41+, CD61+ or CD42b+", clin="Non-specific", morpho="Non-specific", trait="Standard 3+7 induction"),
  "Acute Basophilic Leukemia" = list(cyto="Non-specific", mut="Non-specific", cd="Non-specific", clin="Non-specific", morpho="Basophilic blasts (metachromatic staining with toluidine blue), dystrophic basophils", trait="Standard 3+7 induction"),
  "AML with t(8;21)" = list(cyto="t(8;21)(q22;q22.1)", mut="RUNX1::RUNX1T1, co-mutations in KIT, FLT3, ASXL1", cd="CD34+, CD117+, MPO+, aberrant CD19+ and CD56+", clin="Often favorable prognosis (Core Binding Factor entity)", morpho="Large blasts with basophilic rim, single large 'compass-needle' Auer rod", trait="Standard 3+7 induction, High-dose Cytarabine"),
  "AML with inv(16)" = list(cyto="inv(16)(p13.1q22) or t(16;16)", mut="CBFB::MYH11, co-mutations in KIT, KRAS, NRAS", cd="CD34+, CD117+, CD14+, CD64+, CD11b+, aberrant CD2+", clin="Leukocytosis with myelomonocytic component, CNS involvement", morpho="'Harlequin' eosinophils (basophilic metachromatic granulations with toluidine blue)", trait="Standard 3+7 induction, Gemtuzumab ozogamicin (Mylotarg)"),
  "AML with mutated NPM1" = list(cyto="Normal karyotype in >80% of cases", mut="NPM1 (exon 12 insertion), often associated with FLT3-ITD, DNMT3A, IDH1/2", cd="Often CD34 negative/weak, CD117+, CD33+, HLA-DR+", clin="Favorable outcome if isolated (without FLT3-ITD), monitored via molecular MRD", morpho="Blasts with 'cup-like' nuclear morphology (deep nuclear invagination)", trait="Standard 3+7 induction, Midostaurin if FLT3 associated"),
  "AML with Myelodysplasia-Related Changes (AML-MRC)" = list(cyto="Complex karyotype, monosomy 5 or 7, del(5q)", mut="TP53, ASXL1, RUNX1, SF3B1, SRSF2, U2AF1, ZRSR2", cd="Myeloblasts CD34+, CD117+, frequent aberrant expressions (CD7, CD56)", clin="Elderly patients, history of MDS or chemotherapy (t-AML), very poor prognosis", morpho="Major multilineage dysplasia (micromegakaryocytes, neutrophil degranulation)", trait="CPX-351 (Vyxeos), Venetoclax + Azacitidine, Allogeneic HSCT"),
  "AML with inv(3) or t(3;3)" = list(cyto="inv(3) or t(3;3)", mut="MECOM rearrangement", cd="Non-specific", clin="Male predominance, HIGHLY ADVERSE PROGNOSIS", morpho="Multilineage dysplasia, Thrombocytosis, Clustered micromegakaryocytes", trait="Non-specific"),
  "AML with t(6;9)" = list(cyto="t(6;9)(p23;q34)", mut="DEK::NUP214", cd="Non-specific", clin="Non-specific", morpho="Basophilic blasts + basocytosis (>= 2% in blood/marrow) + multilineage dysplasia", trait="Non-specific"),
  "AML with t(1;22)" = list(cyto="t(1;22)(p13.3;q13.1)", mut="RBM15::MKL1", cd="Non-specific", clin="Infants", morpho="Megakaryoblastic morphology", trait="Non-specific"),
  "AML with t(8;16)" = list(cyto="t(8;16)(p11.2;p13.3)", mut="KAT6A::CREBBP", cd="Non-specific", clin="Non-specific", morpho="Monoblasts (AML M5) + prominent erythrophagocytosis", trait="Non-specific"),
  
  # --- PLASMACYTOID DENDRITIC CELL NEOPLASMS ---
  "Blastic Plasmacytoid Dendritic Cell Neoplasm (BPDCN)" = list(cyto="Monosomy 9, 13, 15, del(5q), del(12p), 6q anomalies, t(6;8) RUNX2::MYC", mut="TET2, ASXL1, ZRSR2, NRAS/KRAS, ETV6, IKZF1, SRSF2", cd="Strong CD123, HLA-DR+, CD4+, CD56+, TCF4 or TCL1 or CD303 or CD304, BadLamp+, FcεRI+, NG2+, ILT7-", clin="Isolated violaceous hyperpigmented skin nodules/macules, marrow involvement (90%), median survival 3 months", morpho="Medium-sized blasts, micro-vacuoles resembling a pearl necklace along the membrane, 'hand-mirror' pseudopods", trait="Tagraxofusp (anti-CD123), ALL-like or Myeloma-like protocols, Allogeneic HSCT"),
  "Mature Plasmacytoid Dendritic Cell Proliferation (MPDCP)" = list(cyto="Non-specific", mut="Ras mutation (MPN/MDS-pDC), RUNX1 (AML-pDC)", cd="CD123++, CD4+, CD56-", clin="Median age 70 years, associated with myeloid neoplasms", morpho="Clonal proliferation of mature pDCs (>= 2%)", trait="Non-specific"),
  
  # --- ACUTE LYMPHOBLASTIC LEUKEMIAS (ALL) ---
  "B-ALL with t(9;22) (Ph+)" = list(cyto="t(9;22)(q34.1;q11.2)", mut="BCR::ABL1 (minor p190 e1a2 transcript dominant), IKZF1", cd="CD19+, CD10+, CD22+, CD79a+, CD66c+, CD25+, TdT+, possible CD13/33+", clin="Incidence increases with age, historically poor prognosis", morpho="Small blasts (L1), reduced basophilic cytoplasm, dense chromatin", trait="TKIs (Dasatinib, Ponatinib) + ALL Protocol + Blinatumomab"),
  "B-ALL BCR::ABL like" = list(cyto="Rearrangements in ABL1, ABL2, JAK2, CRLF2", mut="JAK/STAT or ABL pathway activation, IKZF1 mutation", cd="CD19+, CD10+, CD22+, CD79a+, strong TSLPR (CRLF2)", clin="Adults and children, poor prognosis, weaker response to standard TKIs", morpho="Small to medium blasts, scant basophilic cytoplasm", trait="ALL Protocols + targeted Tyrosine Kinase Inhibitors (TKIs)"),
  "B-ALL with t(5;14) IL3::IGH" = list(cyto="t(5;14)(q31.1;q32.3) IL3::IGH", mut="IL-3 overexpression", cd="CD19+, CD10+, CD13+, CD33+, CD123+", clin="Associated with reactive bone marrow eosinophilia", morpho="Classic ALL blast morphology with concurrent hypereosinophilia", trait="ALL multiagent chemotherapy"),
  "B-ALL with t(12;21) ETV6::RUNX1" = list(cyto="t(12;21)(p13;q22)", mut="ETV6::RUNX1 fusion gene", cd="CD19+, CD10+, weak CD9, weak CD20, CD27+, possible CD13/33+", clin="Children (rare > 25 years old), excellent prognosis", morpho="Small blasts, fine chromatin", trait="Pediatric ALL chemotherapy"),
  "B-ALL with t(1;19) TCF3::PBX1" = list(cyto="t(1;19)(q23;p13.3)", mut="TCF3::PBX1 fusion gene", cd="CD19+, CD10+, weak CD20, negative/weak CD34, strong CD9", clin="Mostly in children, potentially aggressive clinical course", morpho="Medium-sized blasts, occasionally vacuolated cytoplasm", trait="Intensive ALL chemotherapy"),
  "B-ALL with KMT2A rearrangement" = list(cyto="t(4;11)(q21;q23.3) AF4::KMT2A", mut="Fusion gene involving KMT2A (formerly MLL)", cd="CD19+, CD15+, CD65+, NG2+, CD10 negative, CD24 negative, TdT negative", clin="Highly prevalent in infants (< 1 year), massive leukocytosis", morpho="Rare blasts with monoblastic appearance", trait="Intensive chemotherapy, Allogeneic HSCT"),
  "T-ALL (T-cell Acute Lymphoblastic Leukemia)" = list(cyto="TCR rearrangements (TRA/TRD at 14q11.2, TRB at 7q34)", mut="TAL1, NOTCH1, CDKN2A, FBXW7", cd="cCD3+, CD7+, CD2+, weak/absent CD5, weak/neg sCD3, frequent CD4/CD8 coexpression, TdT+, CD1a+ (cortical stage), CD34, CD99, KIT/CD117, weak CD45, aberrant CD79a/CD10/CD13/CD33", clin="Adolescent/young adult, mediastinal (thymic) mass, hyperleukocytosis", morpho="Heterogeneous blasts, convoluted nuclei, high cellular cohesion, 'hand-mirror' shape", trait="Intensive ALL protocol, Nelarabine if refractory"),
  "Early T-cell Precursor (ETP) ALL" = list(cyto="Non-specific", mut="FLT3, NRAS/KRAS, DNMT3A, IDH1, IDH2", cd="cCD3+, CD7+, weak CD5, >= 1 myeloid marker (CD117, CD34, HLA-DR, CD13, CD33, CD11b, CD65)", clin="High risk of induction failure/relapse", morpho="Non-specific", trait="Non-specific"),
  "Mixed Phenotype Acute Leukemia (MPAL)" = list(cyto="t(9;22) BCR::ABL1 or KMT2A rearrangement or ZNF384", mut="FLT3, NRAS/KRAS, IKZF1", cd="Strong co-expression of myeloid (MPO, CD13/33) + Lymphoid (cCD3 or CD19/79a) markers", clin="High leukocytosis, bone marrow failure, poor overall survival", morpho="Heterogeneous blast population (myeloid and lymphoid) or biphenotypic blasts", trait="ALL or AML induction protocols, Allogeneic HSCT"),
  "Acute Undifferentiated Leukemia (AUL)" = list(cyto="Complex karyotype (often -7, del(17p))", mut="SET::NUP214, PHF6, RUNX1, ASXL1, SRSF2, BCOR", cd="Exclusively CD34+, HLA-DR+, TdT+ (cCD3-, MPO-, CD19-, completely lacking B/T/NK markers)", clin="Pancytopenic undifferentiated acute leukemia presentation", morpho="Undifferentiated blasts, MPO negative, total absence of azurophilic granules", trait="Intensive protocol, Allogeneic HSCT"),
  
  # --- MATURE LYMPHOID NEOPLASMS / MULTIPLE MYELOMA ---
  "Mantle Cell Lymphoma (MCL)" = list(cyto="t(11;14)(q13;q32)", mut="IGH::CCND1 (Cyclin D1 overexpression), TP53, ATM", cd="CD5+, CD23-, Cyclin D1+, SOX11+, strong CD20, FMC7+, CD43+, CD200-", clin="Lymphadenopathy, splenomegaly, gastrointestinal involvement (lymphomatous polyposis)", morpho="Atypical lymphocytes, cleaved nuclei resembling a 'bear claw', 'brioche', or 'Pac-Man'", trait="Chemotherapy (R-BAC, R-DHAP), Ibrutinib, anti-CD19 CAR-T cells"),
  "In Situ Mantle Cell Neoplasia" = list(cyto="t(11;14)(q13;q32) IGH::CCND1", mut="Non-specific", cd="CD19+, CD20+, CD79a+, IgD+, BCL2+, Cyclin D1+, variable SOX11, CD5-, CD43-", clin="Incidental finding, localized in lymph nodes and extranodal lymphoid tissues", morpho="Colonization of mantle zones by B cells", trait="Watch and wait"),
  "Follicular Lymphoma (FL)" = list(cyto="t(14;18)(q32;q21.3)", mut="IGH::BCL2, KMT2D, EZH2, CREBBP", cd="CD10+, BCL2+, BCL6+, CD5-, CD23-, CD43-, weak CD19, strong CD20", clin="Indolent lymphadenopathy with a relapsing/remitting course", morpho="Centrocytes (small cells with acutely cleaved 'coffee-bean' nuclei) and centroblasts", trait="Rituximab, Bendamustine, Obinutuzumab, CAR-T in relapse"),
  "Follicular Lymphoma with Unusual Cytological Features (uFL)" = list(cyto="Non-specific", mut="Non-specific", cd="Weak or negative BCL2", clin="Inguinal lymph node mass, diffuse architecture", morpho="Medium-sized cells with immature chromatin / large centrocytes", trait="Adapted to histological grade"),
  "In Situ Follicular Neoplasia" = list(cyto="t(14;18) IGH::BCL2", mut="Non-specific", cd="Strong BCL2+, CD10+/-", clin="Asymptomatic, preserved lymph node architecture, may coexist with other lymphomas", morpho="Presence of B cells confined within germinal centers", trait="Watch and wait"),
  "Pediatric-type Follicular Lymphoma" = list(cyto="Lack of BCL2, BCL6 and c-Myc rearrangements", mut="Non-specific", cd="CD10+, BCL2-, absence of strong IRF4/MUM1 expression", clin="Adolescents or young adults (2–25 years), localized lymphadenopathy", morpho="Non-specific", trait="Surgical excision, reduced-intensity regimens"),
  "Duodenal-type Follicular Lymphoma" = list(cyto="t(14;18) IGH::BCL2", mut="TNFRSF14, EZH2, KMT2D and CREBBP mutations", cd="CD10+, BCL2+, BCL6+", clin="Frequent in the 2nd portion of the duodenum (around the ampulla of Vater), small intestine", morpho="Non-specific", trait="Watch and wait or targeted radiotherapy"),
  
  "Chronic Lymphocytic Leukemia (CLL)" = list(cyto="del(13q), trisomy 12, del(11q), del(17p)", mut="TP53, unmutated IGHV (marker of aggressiveness), NOTCH1, SF3B1", cd="Matutes Score >=4: CD5+, CD23+, weak CD20, weak sIg, CD200+, FMC7-, CD43+", clin="Symmetrical lymphadenopathy, risk of AIHA or ITP, Binet staging", morpho="Monotonous small mature lymphocytes, Gumprecht shadows (smudge cells)", trait="BTK inhibitors (Ibrutinib, Acalabrutinib) or BCL2 inhibitors (Venetoclax)"),
  "Burkitt Lymphoma" = list(cyto="t(8;14)(q24;q32) or variant t(2;8) / t(8;22)", mut="IGH::MYC, TCF3, ID3, TP53", cd="CD10+, BCL6+, Ki67 approaching 100%, BCL2 negative, strong sIgM, CD43+", clin="Absolute medical emergency, rapidly growing tumor mass (often abdominal), exceedingly high LDH", morpho="Medium-sized cells, intense basophilia, 'starry sky' appearance, numerous nuclear/lipid vacuoles", trait="Highly intensive polychemotherapy (CODOX-M/IVAC)"),
  "Hairy Cell Leukemia (HCL)" = list(cyto="Non-contributory karyotype", mut="BRAF V600E (exon 15)", cd="CD103+, CD123+, CD25+, CD11c+, strong CD200, strong sIg, Annexin A1+, VE1+", clin="Pancytopenia, massive splenomegaly without lymphadenopathy, severe infectious risk", morpho="Oval lymphocytes with circumferential, 'fried-egg' or 'combed' cytoplasmic projections", trait="Purine analogs (Cladribine, Pentostatin) or Vemurafenib"),
  "Waldenström Macroglobulinemia" = list(cyto="del(6q) possible", mut="MYD88 L265P (>90%), CXCR4 (associated with resistance)", cd="Monoclonal IgM, CD19+, CD20+, strong CD79b, CD5-, CD10-, CD138-, CD38+/-", clin="Hyperviscosity syndrome, anti-MAG IgM neuropathy, altered fundus (sausage-like retinal veins)", morpho="Polymorphic infiltrate of small lymphocytes, plasmacytoid cells, and plasma cells; rouleaux formation, associated mast cells", trait="Rituximab, Ibrutinib, Alkylating agents"),
  
  "Diffuse Large B-Cell Lymphoma (DLBCL-NOS)" = list(cyto="MYC, BCL2, BCL6 rearrangements", mut="MYD88, CD79b, TP53, EZH2 (GC subtype)", cd="CD19+, CD20+, CD79a+, CD10+/- (GCB or ABC origin), MUM1+/-", clin="Most frequent aggressive lymphoma, rapidly enlarging tumor masses, 30-40% extranodal involvement", morpho="Large atypical cells, dispersed chromatin, prominent nucleoli (centroblastic or immunoblastic variants)", trait="R-CHOP, CAR-T cells"),
  "Primary Mediastinal Large B-Cell Lymphoma (PMBCL)" = list(cyto="Non-specific", mut="Non-specific", cd="CD10-, BCL6+, MUM1+, CD23+, partial CD30", clin="Bulky mediastinal mass, young female predominance (20-40 years)", morpho="Non-specific", trait="R-CHOP +/- radiotherapy"),
  "T-cell/Histiocyte-rich Large B-Cell Lymphoma (THRLBCL)" = list(cyto="Non-specific", mut="Non-specific", cd="CD10-, BCL6+, MUM1+, CD68+ CD163+ microenvironment", clin="Male predominance, 66% 5-year survival rate", morpho="Low proportion (<10%) of large B cells scattered within a background rich in CD4+ T cells and histiocytes", trait="R-CHOP"),
  "Plasmablastic Lymphoma" = list(cyto="MYC rearrangement", mut="TP53, CARD11, JAK/STAT, NOTCH", cd="CD20-, PAX5-, CD45-, EBER+, CD138+, CD38+, MUM1+", clin="Oral/nasal cavity, GI tract, severe immunosuppression (HIV)", morpho="Plasmablastic morphology, Ki-67 > 90%", trait="Intensive chemotherapy"),
  "ALK-positive Large B-Cell Lymphoma" = list(cyto="t(2;17) ALK::CLTC", mut="Non-specific", cd="CD19-, CD20-, CD22-, EBER-, ALK+, CD138+, MUM1+, EMA+", clin="Median age 35-38 years, nodal involvement (75%), brain, liver", morpho="Non-specific", trait="Chemotherapy + ALK inhibitors"),
  "Large B-Cell Lymphoma with IRF4 rearrangement (LBCL-IRF4-R)" = list(cyto="IG::IRF4", mut="Non-specific", cd="CD10+, BCL6+, MUM1+, BCL2+", clin="Children/young adults, Waldeyer's ring, cervical lymph nodes", morpho="Non-specific", trait="R-CHOP"),
  "High-Grade B-Cell Lymphoma (MYC/BCL2 Double/Triple Hit)" = list(cyto="BCL2::IGH t(14;18) AND MYC rearrangement (8q24)", mut="KMT2D, CREBBP, EZH2", cd="Non-specific", clin="Bone marrow or CNS dissemination, VERY POOR PROGNOSIS", morpho="Non-specific", trait="R-EPOCH"),
  "High-Grade B-Cell Lymphoma with 11q aberration (HGBCL-11q)" = list(cyto="Complex 11q aberration, lacking MYC translocation", mut="Non-specific", cd="CD10+, BCL6+, BCL2-, MYC+, CD56+", clin="Children/adolescents and adults < 60 years", morpho="High Ki-67 index (> 90%)", trait="Intensive polychemotherapy"),
  
  "MGUS (Monoclonal Gammopathy of Undetermined Significance)" = list(cyto="Non-specific", mut="MYD88 p.L265P mutation possible (if IgM)", cd="Clonal plasma cells CD38+, CD138+, CD19-, weak CD45", clin="Absence of CRAB criteria, M-protein spike < 30 g/L", morpho="< 10% bone marrow infiltration by lymphoplasmacytic or plasma cells", trait="Watch and wait"),
  "AL Amyloidosis" = list(cyto="Non-specific", mut="Non-specific", cd="Non-specific", clin="Heart failure with preserved ejection fraction, proteinuria, peripheral neuropathy, hepatomegaly", morpho="Extracellular deposition of amyloid fibrils ('apple-green' birefringence under Congo Red stain)", trait="Chemotherapy, Autologous Stem Cell Transplant"),
  "Multiple Myeloma (MM)" = list(cyto="t(11;14), t(4;14), t(14;16), del(17p), 1q gain", mut="KRAS, NRAS, BRAF, TP53", cd="CD138+, CD38+, CD56+, CD19-, CD45-, sIg-, CD28+ (poor prognosis)", clin="CRAB criteria, lytic 'punched-out' bone lesions, hypercalcemia, renal failure", morpho="Dystrophic plasma cells (multinucleated, prominent nucleoli), flame cells, Mott cells, striking rouleaux formation", trait="Bortezomib, Lenalidomide, Daratumumab (anti-CD38), anti-BCMA CAR-T cells"),
  
  "Sézary Syndrome" = list(cyto="Highly complex karyotype", mut="TP53, PLCG1 alterations", cd="CD4+, CD7-, CD26-, PD-1+, CCR4+ (therapeutic target), TCRab+", clin="Diffuse erythroderma (>80%), intense refractory pruritus, early lymphadenopathy", morpho="Large lymphocytes with a highly convoluted, 'cerebriform' (brain-like) or 'ghost-like' nucleus", trait="Extracorporeal photopheresis, Mogamulizumab (anti-CCR4)"),
  "Adult T-Cell Leukemia/Lymphoma (ATLL)" = list(cyto="Complex anomalies", mut="Clonal HTLV-1 viral integration", cd="CD2+, CD3+, CD5+, CD4+, CD25+, CCR4+, FOXP3+, CD7-", clin="Linked to HTLV-1 virus, severe hypercalcemia, cutaneous lesions (nodules/papules), aggressive clinical course", morpho="Medium-sized lymphocytes with irregular polylobated nuclei ('flower cells'), scant basophilic cytoplasm", trait="Zidovudine/Interferon, Mogamulizumab, Allogeneic HSCT"),
  "Anaplastic Large Cell Lymphoma (ALCL)" = list(cyto="t(2;5)(p23;q35) ALK::NPM1 (if ALK+)", mut="JAK1/3, STAT3", cd="Strong CD30+, CD43+, MUM1+, EMA+, CD4+, variable CD3/CD5 expression", clin="Multiple lymphadenopathies, frequent extranodal involvement (skin, liver), B symptoms", morpho="Giant pleomorphic cells, 'horseshoe' or 'ring-shaped' nuclei (hallmark cells), prominent nucleolus, abundant cytoplasm", trait="Brentuximab vedotin, CHOP Protocol"),
  "Hepatosplenic T-Cell Lymphoma" = list(cyto="Isochromosome 7q i(7q), trisomy 8", mut="STAT3, STAT5b", cd="CD2+, CD3+, CD16+, CD56+, TCR gamma-delta+ (or ab), CD4-, CD5-, CD8-", clin="Striking hepatosplenomegaly, typically without lymphadenopathy, rapidly fatal progression", morpho="Medium size, elongated nucleus, fine chromatin, basophilic scalloped cytoplasm", trait="Intensive chemotherapy, HSCT"),
  "Angioimmunoblastic T-Cell Lymphoma (AITL)" = list(cyto="Trisomy 3, trisomy 5, trisomy 21", mut="TET2, DNMT3A, RHOA (G17V), IDH2", cd="CD4+, PD-1+ (CD279+), ICOS+, CXCL13+, BCL6+, CD10+", clin="Proliferation of T follicular helper (TFH) cells, skin rash, polyclonal hypergammaglobulinemia, AIHA", morpho="Polymorphic infiltrate (eosinophils, plasma cells, EBV+ immunoblasts) effacing lymph node architecture", trait="Chemotherapy (CHOP), Corticosteroids, Stem cell transplant"),
  
  "T-cell Large Granular Lymphocytic Leukemia (T-LGLL)" = list(cyto="Non-specific", mut="STAT3 (30%), STAT5B", cd="CD3+, CD4-, CD8+, weak/neg CD5, CD27-, CD28-, CD45RA+, CD95+, CD178+, TCRab", clin="Neutropenia (Fas-ligand mediated increased apoptosis), splenomegaly, autoimmune diseases (FELTY syndrome, Rheumatoid Arthritis)", morpho="Medium-sized lymphocytes, abundant cytoplasm, prominent azurophilic granules", trait="Immunosuppressive agents (Methotrexate, Cyclophosphamide, Cyclosporine)"),
  "NK-cell Large Granular Lymphocytic Leukemia (NK-LGLL)" = list(cyto="Non-specific", mut="STAT3, STAT5b, TET2, TNFAIP3, CCL22", cd="CD3-, CD2+, CD4-, CD8+, variable CD56/CD57, restricted KIR repertoire", clin="Recurrent bacterial infections, severe neutropenia", morpho="Medium-sized lymphocytes, abundant cytoplasm, prominent azurophilic granules", trait="Immunosuppressive therapy"),
  "Aggressive NK-Cell Leukemia" = list(cyto="del(6), del(7q), del(11q), del(17p), +(1q)", mut="Strongly EBV-associated", cd="Non-specific", clin="Prevalent in Asian populations, severe systemic symptoms, hepatosplenomegaly, DIC, Hemophagocytic Lymphohistiocytosis (HLH)", morpho="Non-specific", trait="Non-specific"),
  "Extranodal NK/T-Cell Lymphoma, nasal type" = list(cyto="del(4q), del(5q), del(6q), del(7q), del(11q), del(15q)", mut="STAT3, STAT5, JAK3, EBV-associated", cd="Non-specific", clin="Prevalent in Asia and Central/South America, adult male predominance", morpho="Non-specific", trait="Localized radiation, asparaginase and gemcitabine")
)

encadres_gris <- list(
  list(q="Which immunophenotypic score is used to validate the diagnosis of CLL in flow cytometry?", a="The Matutes Score (CD5+, CD23+, FMC7-, weak CD22/79b, weak sIg)", d=c("The Hasenclever Score", "The OGATA Score", "The RED SCORE", "The Sokal Score")),
  list(q="What are the 'CRAB' clinical-biological criteria indicating active Multiple Myeloma?", a="HyperCalcemia, Renal Insufficiency, Anemia, Bone lytic lesions", d=c("Cephalea, Rash, Anorexia, Bicytopenia", "Cardiomegaly, Retinopathy, Adenopathy, Blastosis", "Coagulopathy, Renal, Asthenia, Bacteremia")),
  list(q="Which pathological cells must be excluded from the lymphocyte count when establishing a complete blood count formula?", a="Sézary cells, Hairy cells, Large B-cell lymphoma cells, Prolymphocytes", d=c("Monocytes, Plasma cells, Macrophages, Histiocytes", "Erythroblasts, Dwarf megakaryocytes, Giant platelets", "Blasts, Promyelocytes, Myelocytes, Metamyelocytes")),
  list(q="What are the major biological parameters of Hemophagocytic Lymphohistiocytosis (HLH) / Macrophage Activation Syndrome?", a="Ferritin >500 µg/L, Platelets <100 G/L, Fibrinogen <1.5 g/L", d=c("Ferritin <30, Platelets >500, Fibrinogen >4", "LDH >1000, Schistocytes >1%, Undetectable haptoglobin", "CRP >100, WBC >50, PCT >2")),
  list(q="In flow cytometry, which 4 parameters constitute the OGATA score to support an MDS diagnosis?", a="% of CD34+, % of B-cell progenitors (hematogones), Lymphocyte/myeloblast ratio, Neutrophil SSC mode", d=c("% of CD19+, % of CD5+, Kappa/Lambda ratio, FSC mode", "Expression of CD103, CD25, CD11c, CD123", "Expression of CD138, CD38, CD56, CD45")),
  list(q="Faced with an unexplained THROMBOCYTOPENIA, which anomalies should you immediately look for on a blood smear?", a="Giant platelets, Schistocytes (TMA), Malaria, Platelet clumps", d=c("Myeloid blasts, Dacryocytes, Erythroblasts", "Microcytosis, Hypochromia, Hypersegmented neutrophils", "Gumprecht shadows, Sézary cells")),
  list(q="[RED FRAME] Which fusion transcript defines Chronic Myeloid Leukemia (CML)?", a="t(9;22)(q34;q11) BCR::ABL1 with p210 transcript", d=c("t(15;17) PML::RARA with p190 transcript", "t(14;18) IGH::BCL2 with p210 transcript", "inv(16) CBFB::MYH11 with minor transcript")),
  list(q="[RED FRAME] Which mutation is pathognomonic of Waldenström Macroglobulinemia in >90% of cases?", a="MYD88 L265P mutation", d=c("BRAF V600E mutation", "CXCR4 WHIM mutation", "SF3B1 K700E mutation")),
  list(q="[RED FRAME] What are the three classic 'driver' mutations defining Ph- MPNs (ET, PV, PMF)?", a="JAK2 V617F (or exon 12), CALR, MPL", d=c("TET2, ASXL1, DNMT3A", "FLT3-ITD, NPM1, CEBPA", "NOTCH1, TP53, ATM")),
  list(q="[RED FRAME] In AML with NPM1 mutation, which co-mutation dictates the addition of targeted therapy?", a="The presence of an FLT3-ITD transcript", d=c("The presence of a DNMT3A mutation", "The presence of a chromosome 7 deletion", "The loss of TP53 function")),
  list(q="[RED FRAME] Which genetic rearrangement characterizes Follicular Lymphoma?", a="t(14;18)(q32;q21) IGH::BCL2", d=c("t(11;14)(q13;q32) IGH::CCND1", "t(8;14)(q24;q32) IGH::MYC", "t(12;21)(p13;q22) ETV6::RUNX1")),
  list(q="Which parameters are measured by the RED SCORE to guide an MDS diagnosis via flow cytometry?", a="CD71 CV, CD36 CV, and Hemoglobin", d=c("Neutrophil SSC, Lympho/myeloblast ratio, % CD34+", "Reticulocyte count, MCV, LDH", "SF3B1 mutation, Karyotype, B12")),
  list(q="In flow cytometry, what is the positivity threshold of the OGATA Score for MDS?", a="Score >= 2", d=c("Score = 4", "Score >= 3", "Score = 1")),
  
  list(q="What are the cytological signs of erythroblast dysplasia in MDS?", a="Macrocytosis, Karyorrhexis, Internuclear bridging, Multinuclearity, Vacuolization, Defective hemoglobinization ('flaky' appearance), Ring sideroblasts", d=c("Gumprecht shadows", "Circumferential villi", "Mott cells", "Faggot cells")),
  list(q="What are the cytological signs of neutrophil dysplasia in MDS?", a="Hyposegmentation (pseudo-Pelger-Huët), Karyoschisis, Ring/mirror nuclei, Degranulation or Hypergranulation, Döhle bodies, Pseudo-Chédiak anomaly, Vacuolization", d=c("Cleaved 'bear-claw' nucleus", "Cerebriform or ghost-like nucleus", "Cup-like nucleus", "Harlequin eosinophils")),
  list(q="How is MDS-EB2 defined according to cytological anomalies?", a="5% < Blood blasts < 19% OR 10% < Marrow blasts < 19% OR Auer rods", d=c("2% < Blood blasts < 4% OR 5% < Marrow blasts < 9%", "< 2% blood blasts and < 5% marrow blasts", "20% < Marrow blasts < 30%", "Exclusive presence of micromegakaryocytes")),
  list(q="What is the differential diagnosis of dysplasia with pseudo-Pelger-Huët neutrophils outside of MDS?", a="Iatrogenic: CellCept, tacrolimus, decitabine, valproic acid, ganciclovir", d=c("Lead and arsenic poisoning", "Vitamin B12/Folate deficiency", "HIV infection", "Zinc toxicity and copper deficiency")),
  list(q="Which secondary etiologies can cause ring sideroblasts on Perls stain outside of hematologic malignancies?", a="Chronic alcoholism, Lead/arsenic poisoning, Vitamin B1/B6/B9/B12 deficiencies, Isoniazid", d=c("CellCept or tacrolimus treatment", "HIV infection", "G-CSF therapy", "EBV or CMV infection")),
  list(q="Which secondary etiology causes flaky cytoplasm and basophilic stippling alongside ring sideroblasts?", a="Lead and arsenic poisoning", d=c("Methotrexate treatment", "CMV infection", "Severe malnutrition", "Zinc toxicity")),
  list(q="In flow cytometry, which progenitor profile confers a poor prognosis in MDS?", a="MPP (Multipotent progenitor) profile", d=c("CMP (Common myeloid progenitor) profile", "MEP (Megakaryocyte-erythroid progenitor) profile", "GMP (Granulocyte-monocyte progenitor) profile")),
  list(q="Which molecular mutations in MDS are linked to spliceosome alterations?", a="SF3B1, SRSF2, U2AF1, ZRSR2", d=c("TET2, DNMT3A", "ASXL1, EZH2", "TP53, RUNX1", "JAK2, CALR, MPL")),
  list(q="How is CHIP (Clonal Hematopoiesis of Indeterminate Potential) defined?", a="Somatic mutation with VAF > 2% (4% if X-linked) + absence of hematologic malignancy + absence of cytopenia", d=c("Somatic mutation with VAF > 2% + cytopenia + absence of hematologic malignancy", "Presence of an isolated PNH clone > 50%", "Peripheral blood blasts > 2%", "Clonal proliferation of mature pDCs")),
  list(q="How is CCUS (Clonal Cytopenia of Undetermined Significance) defined?", a="Somatic mutation with VAF > 2% (4% if X-linked) + neutropenia, anemia or thrombocytopenia + absence of hematologic malignancy", d=c("Somatic mutation with VAF > 2% + absence of hematologic malignancy + absence of cytopenia", "Pancytopenia with isolated del(5q)", "1-3 lineage dysplasia without mutation", "Bone marrow blasts > 5%")),
  list(q="Which inflammatory diseases or vasculitides can be associated with MDS?", a="Behcet's disease, Giant cell arteritis (Horton), Takayasu, Polyarteritis nodosa, Sweet syndrome", d=c("Infective endocarditis, Brucellosis", "Celiac disease, Bullous pemphigoid", "Sarcoidosis, Kikuchi disease", "Löffler's disease, DRESS syndrome")),
  list(q="What are the clinical and biological signs of VEXAS syndrome associated with MDS?", a="Chondritis of the nose and ears, vasculitis, vacuoles in proerythroblasts and immature granulocytes, UBA1 mutation", d=c("Löffler endocarditis, endomyocardial fibrosis, PDGFRA mutation", "Urticaria pigmentosa, Darier's sign, KIT mutation", "Aquagenic pruritus, erythromelalgia, JAK2 mutation", "Oral and genital aphthosis, posterior uveitis, trisomy 8")),
  list(q="In MDS, which mutations is hemolysis preferentially associated with?", a="U2AF1 and EZH2 mutations", d=c("SF3B1 and TET2 mutations", "ASXL1 and DNMT3A mutations", "JAK2 and MPL mutations", "NPM1 and FLT3 mutations")),
  list(q="Which hematologic malignancies are associated with the presence of ring sideroblasts on Perls stain?", a="MDS, AML with myelodysplasia-related changes, MDS/MPN overlap syndromes, SF3B1 gene mutation", d=c("CML, ET, PV, PMF", "B-ALL, T-ALL, MPAL", "CLL, Mantle Cell Lymphoma, Follicular Lymphoma", "Multiple Myeloma, AL Amyloidosis, MGUS")),
  list(q="Which cytological anomalies are typically observed in Vitamin B12 or Folate deficiency?", a="Megaloblastosis, hypersegmentation of neutrophils, giant metamyelocytes with band-like nuclei", d=c("Pseudo-Pelger-Huët neutrophils, vacuoles, micromegakaryocytes", "Erythroblasts with basophilic stippling, flaky cytoplasm", "Hypergranular neutrophils, dystrophic monocytes, Döhle bodies", "Target cells, elliptocytes, severe hypochromia")),
  list(q="Which marrow cytological anomalies can be induced by G-CSF therapy?", a="Neutrophils with vacuoles, hyposegmentation, toxic granulations, Döhle bodies", d=c("Erythroblastic gigantism and giant metamyelocytes", "Micromegakaryocytes and major dyserythropoiesis", "Hyperbasophilic lymphocytes and vacuolated monocytes", "Dystrophic plasma cells and sea-blue histiocytes")),
  list(q="What is the prognostic impact of a PNH clone associated with MDS?", a="Favorable prognosis with better response to transplantation and improved overall survival", d=c("Poor prognosis with rapid transformation to acute leukemia", "No impact on survival or therapeutic response", "Increased risk of deep vein thrombosis exclusively", "Systematic progression towards aggressive systemic mastocytosis")),
  list(q="Which acute myeloid leukemia is associated with multilineage dysplasia, thrombocytosis, clustered micromegakaryocytes, and a MECOM rearrangement?", a="AML with inv(3) or t(3;3)", d=c("AML with t(6;9)", "AML with t(1;22)", "AML with t(8;16)", "AML with t(8;21)")),
  list(q="Which acute myeloid leukemia is characterized by megakaryoblastic proliferation in infants?", a="AML with t(1;22) RBM15::MKL1", d=c("AML with t(8;16) KAT6A::CREBBP", "AML with t(6;9) DEK::NUP214", "AML with inv(3) MECOM", "AML with inv(16) CBFB::MYH11")),
  list(q="Which acute myeloid leukemia combines monoblasts (AML M5) and images of prominent erythrophagocytosis?", a="AML with t(8;16) KAT6A::CREBBP", d=c("AML with t(1;22) RBM15::MKL1", "AML with inv(3) MECOM", "AML with t(6;9) DEK::NUP214", "AML with NPM1 mutation")),
  list(q="Which acute myeloid leukemia is characterized by the presence of basophilic blasts and basocytosis associated with multilineage dysplasia?", a="AML with t(6;9) DEK::NUP214", d=c("AML with t(8;16) KAT6A::CREBBP", "AML with t(1;22) RBM15::MKL1", "AML with inv(3) MECOM", "AML with inv(16) CBFB::MYH11")),
  list(q="Which immunophenotype is characteristic of Blastic Plasmacytoid Dendritic Cell Neoplasms (BPDCN)?", a="Strong CD123, HLA-DR+, CD4+, CD56+, TCF4 or TCL1 or CD303 or CD304, ILT7-", d=c("Strong CD117+, FcεRI+, CD25+, CD2+, CD30+", "CD13+, CD33+, CD11b+, CD15+, CD117+, CD34+", "CD19+, CD10+, CD22+, CD79a+, TdT+", "CD103+, CD123+, CD25+, CD11c+, strong CD200")),
  list(q="Which pathology is characterized by a clonal proliferation of mature plasmacytoid dendritic cells (CD123++ CD4+ CD56-) associated with a Ras or RUNX1 mutation?", a="Mature Plasmacytoid Dendritic Cell Proliferation (MPDCP)", d=c("Blastic Plasmacytoid Dendritic Cell Neoplasm (BPDCN)", "Acute Basophilic Leukemia", "Granulocytic Sarcoma", "Hepatosplenic T-Cell Lymphoma"))
)

# ------------------------------------------------------------------------------
# 4. USER INTERFACE (UI)
# ------------------------------------------------------------------------------
my_theme <- bs_theme(version = 5, bootswatch = "darkly", primary = "#E74C3C", success = "#18BC9C")

ui <- page_navbar(
  theme = my_theme, title = "🩸 HEMOCM", id = "nav",
  
  tags$head(
    tags$link(href="https://fonts.googleapis.com/css2?family=VT323&display=swap", rel="stylesheet"),
    uiOutput("dynamic_bg"), 
    tags$style(HTML("
      /* --- MOBILE RESPONSIVE CSS & FONTS --- */
      * { font-family: 'VT323', monospace !important; }
      body { font-size: 20px; line-height: 1.5; color: #eee; }
      h2 { font-size: 28px; color: #ff3333; text-align: center; } 
      h3 { font-size: 22px; color: #fff; padding: 15px; background: #1a1a1a; border: 2px dashed #444; font-weight:normal;}
      
      #user_choice { width: 100% !important; }
      .radio-container { background: #000; padding: 15px; border: 4px solid #444; margin-bottom: 10px; width: 100%; }
      .shiny-options-group { display: block !important; width: 100% !important; }
      .form-check { display: block !important; width: 100% !important; padding-left: 35px !important; margin-bottom: 15px !important; border-bottom: 1px dashed #444 !important; padding-bottom: 10px !important; min-height: 30px !important;}
      .form-check-input { float: left !important; margin-left: -35px !important; margin-top: 8px !important; position: absolute !important;}
      .form-check-label { display: block !important; width: 100% !important; white-space: normal !important; word-wrap: break-word !important; text-align: left !important; color: #0dcaf0 !important; font-size: 20px !important; line-height: 1.4 !important; cursor: pointer !important;}
      .form-check-input:checked + .form-check-label { color: #fff !important; text-shadow: 0px 0px 8px #18BC9C !important; }
      
      /* --- BACKGROUND REMAINS SOLID BLACK --- */
      .doom-viewport { border: 6px solid #555; height: 60vh; min-height: 400px; text-align: center; position: relative; overflow: hidden; margin-top: 15px; background: #000 !important; }
      
      .btn-feu {
        position: absolute; left: 10px; bottom: 10px; width: 80px; height: 80px; border-radius: 50%;
        background-color: #E74C3C; color: white; font-size: 22px; font-weight: bold;
        border: 4px solid #fff; box-shadow: 0 0 15px #e74c3c; z-index: 10;
        text-align: center; display: flex; align-items: center; justify-content: center; cursor: pointer; transition: all 0.1s;
      }
      .btn-feu:active { transform: scale(0.95); box-shadow: 0 0 5px #e74c3c; }
      
      @keyframes breathing { 0% { transform: scaleX(1) scaleY(1); } 50% { transform: scaleX(1.02) scaleY(0.98); } 100% { transform: scaleX(1) scaleY(1); } }
      .anim-breathing { animation: breathing 3s infinite ease-in-out; transform-origin: bottom center; }
      .monster-wrapper { position: absolute; top: 10%; left: 50%; transform: translateX(-50%); transform-origin: top center; z-index: 2; width: 100%; text-align: center; pointer-events: none; }
      .gen-monster { mix-blend-mode: screen; pointer-events: none; max-height: 45vh; max-width: 90%; }
      
      .muzzle-flash { position: absolute; bottom: 40vh; left: 50%; transform: translateX(-50%); opacity: 0; z-index: 5 !important; pointer-events: none; }
      .flash-default { width: 300px; height: 300px; background: radial-gradient(circle, #fff 10%, #ffeb3b 30%, #e74c3c 60%, transparent 80%); border-radius: 50%; filter: blur(5px) contrast(1.5); }
      .flash-laser { width: 45px; height: 1000px; bottom: 0px; background: #3498db; filter: blur(3px) brightness(2.5); box-shadow: 0 0 40px #3498db, 0 0 80px #0dcaf0; }
      .flash-machinegun { width: 200px; height: 200px; background: radial-gradient(circle, #fff, #f1c40f, transparent); border-radius: 50%; filter: blur(2px); }
      .flash-bazooka { width: 600px; height: 600px; background: radial-gradient(circle, #fff, #f39c12, #c0392b, transparent); border-radius: 50%; filter: blur(8px) contrast(2); }
      
      /* --- WEAPON BOB ANIMATION --- */
      @keyframes weapon-bob {
        0% { transform: translateY(0px); }
        25% { transform: translateY(15px) rotate(1deg); }
        50% { transform: translateY(0px); }
        75% { transform: translateY(15px) rotate(-1deg); }
        100% { transform: translateY(0px); }
      }
      .weapon-bob-container {
        position: absolute; bottom: 0; left: 0; width: 100%; height: 100%; z-index: 4; pointer-events: none;
        animation: weapon-bob 2s infinite ease-in-out;
      }
      .gen-weapon { position: absolute; bottom: -5vh; left: 50%; transform: translateX(-50%); height: 40vh; transition: all 0.1s; color: red; mix-blend-mode: normal; pointer-events: none; }
      
      @keyframes flash-bang { 0% { opacity: 1; transform: translateX(-50%) scale(0.2); } 30% { opacity: 1; transform: translateX(-50%) scale(1.2); } 100% { opacity: 0; transform: translateX(-50%) scale(1.5); } }
      .action-shoot .muzzle-flash { animation: flash-bang 0.4s ease-out forwards; }
      
      .doom-hud { display: flex; justify-content: space-between; background: #333; border: 4px solid #222; padding: 5px; color: #fff; text-shadow: 2px 2px 0px #000; margin-bottom: 10px; flex-wrap: wrap;}
      .hud-box { text-align: center; background: #111; padding: 5px; border: 2px solid #000; flex: 1 1 22%; margin: 2px; }
      .hud-label { color: #f39c12; font-size: 16px; margin-bottom: 2px; font-weight: bold;}
      .hud-val { font-size: 20px; font-weight: bold; }
      .monster-hp-bar { height: 8px; background: #e74c3c; border: 1px solid #000; margin-top: 5px; transition: width 0.3s;}
      
      .trophy-card { text-align: center; padding: 10px; border: 4px solid #fff; box-shadow: 4px 4px 0px #000; margin-bottom: 15px; height: 160px; display: flex; flex-direction: column; justify-content: center;}
      .trophy-title { font-size: 20px; font-weight: bold; margin-bottom: 5px; line-height: 1.4; color: #f1c40f; text-shadow: 2px 2px 0px #000;}
      .trophy-desc { font-size: 16px; line-height: 1.4; color: #fff; font-weight: bold; text-shadow: 1px 1px 0px #000;}
      .trophy-locked { filter: grayscale(100%) brightness(30%); border-color: #444 !important; background-image: none !important; background-color: #111 !important; box-shadow: none;}
      
      /* --- INNER DARK SHADOWS FOR FLASHES --- */
      @keyframes flash-red-inner { 0% { box-shadow: inset 0 0 150px #ff0000; } 100% { box-shadow: inset 0 0 0px #000; } }
      @keyframes flash-green-inner { 0% { box-shadow: inset 0 0 150px #18BC9C; } 100% { box-shadow: inset 0 0 0px #000; } }
      .anim-flash-red { animation: flash-red-inner 0.6s ease-out forwards; }
      .anim-flash-green { animation: flash-green-inner 0.6s ease-out forwards; }
      
      @keyframes monster-hit { 0% { filter: brightness(2) sepia(1) hue-rotate(-50deg) saturate(5); } 100% { filter: none; } }
      @keyframes monster-attack { 0% { transform: translateX(-50%) scale(1); } 50% { transform: translateX(-50%) scale(1.5) translateY(50px); filter: brightness(0.5) sepia(1) hue-rotate(-50deg) saturate(5);} 100% { transform: translateX(-50%) scale(1); } }
      
      @keyframes gore-head { 0% { transform: translateX(-50%); clip-path: polygon(0 0, 100% 0, 100% 100%, 0 100%); } 100% { transform: translateX(-50%) translateY(80px); clip-path: polygon(0 40%, 100% 40%, 100% 100%, 0 100%); opacity:0;} }
      @keyframes gore-arm { 0% { transform: translateX(-50%); clip-path: polygon(0 0, 100% 0, 100% 100%, 0 100%); } 100% { transform: translateX(-50%) translateX(50px) rotate(20deg); clip-path: polygon(30% 0, 100% 0, 100% 100%, 30% 100%); opacity:0;} }
      @keyframes gore-explode { 0% { transform: translateX(-50%) scale(1); filter: brightness(2); } 100% { transform: translateX(-50%) scale(0.1) translateY(150px); filter: brightness(0); opacity: 0; } }
      @keyframes screen-shake { 0% { transform: translate(1px, 1px) rotate(0deg); } 10% { transform: translate(-10px, -10px) rotate(-2deg); } 20% { transform: translate(-15px, 0px) rotate(2deg); } 30% { transform: translate(15px, 10px) rotate(0deg); } 40% { transform: translate(5px, -5px) rotate(2deg); } 50% { transform: translate(-5px, 10px) rotate(-2deg); } 60% { transform: translate(-15px, 5px) rotate(0deg); } 70% { transform: translate(15px, 1px) rotate(-2deg); } 80% { transform: translate(-5px, -5px) rotate(2deg); } 90% { transform: translate(5px, 10px) rotate(0deg); } 100% { transform: translate(1px, -1px) rotate(0deg); } }
      
      @keyframes gun-recoil { 0% { transform: translateX(-50%) translateY(0); } 50% { transform: translateX(-50%) translateY(60px) rotate(5deg); } 100% { transform: translateX(-50%) translateY(0); } }
      @keyframes gun-recoil-mg { 0% { transform: translateX(-50%) translateY(0); } 20% { transform: translateX(-48%) translateY(15px); } 40% { transform: translateX(-52%) translateY(10px); } 60% { transform: translateX(-48%) translateY(20px); } 80% { transform: translateX(-50%) translateY(10px); } 100% { transform: translateX(-50%) translateY(0); } }
      
      .action-shoot .gen-weapon { animation: gun-recoil 0.4s ease-out; }
      .action-shoot .flash-machinegun ~ .weapon-bob-container .gen-weapon { animation: gun-recoil-mg 0.4s ease-out; } 
      
      .action-hit .monster-wrapper { animation: monster-hit 0.3s forwards; }
      .gore-head { animation: gore-head 0.5s forwards; }
      .gore-arm { animation: gore-arm 0.5s forwards; }
      .gore-explode { animation: gore-explode 0.4s forwards; }
      .action-hurt { animation: screen-shake 0.4s cubic-bezier(.36,.07,.19,.97) both; } 
      .action-hurt .monster-wrapper { animation: monster-attack 0.4s; }
      
      /* Media Query for very small screens */
      @media (max-width: 576px) {
        .hud-val { font-size: 16px; }
        h3 { font-size: 18px; }
        .form-check-label { font-size: 16px !important; }
        
        .monster-wrapper { top: 2% !important; }
        .gen-monster { max-height: 40vh !important; }
        .gen-weapon { height: 35vh !important; bottom: -2vh !important; }
        
        .muzzle-flash { bottom: 20vh !important; }
      }
    "))
  ),
  
  nav_panel("📝 SURVIVAL", fluidRow(
    column(12, class="col-md-3 order-2 order-md-1", uiOutput("loot_panel")), 
    column(12, class="col-md-9 order-1 order-md-2", uiOutput("game_ui"))     
  )),
  nav_panel("🏆 TROPHIES", fluidRow(column(12, h2("Trophy Board"), 
                                           fluidRow(column(3, uiOutput("tr1")), column(3, uiOutput("tr2")), column(3, uiOutput("tr3")), column(3, uiOutput("tr4"))),
                                           fluidRow(column(3, uiOutput("tr5")), column(3, uiOutput("tr6")), column(3, uiOutput("tr7")), column(3, uiOutput("tr8"))),
                                           fluidRow(column(3, uiOutput("tr9")), column(3, uiOutput("tr10")), column(3, uiOutput("tr11")), column(3, uiOutput("tr12"))),
                                           fluidRow(column(3, uiOutput("tr13")), column(3, uiOutput("tr14")), column(3, uiOutput("tr15")), column(3, uiOutput("tr16")))
  ))), 
  nav_panel("📊 STATS", fluidRow(column(10, offset=1, div(class="card mt-4", style="background: rgba(34,34,34,0.9);", h2("Mission Report"), plotOutput("stats_plot"), hr(), h2("Weakness"), uiOutput("weakness_recommendation")))))
)

# ------------------------------------------------------------------------------
# 5. SERVER LOGIC
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- ANTI-REPETITION SYSTEM ---
  asked_questions <- reactiveVal(character())
  
  history_data <- reactiveVal(data.frame(id=integer(), category=character(), correct=logical(), stringsAsFactors=FALSE))
  if (file.exists(HIST_FILE)) { try({ saved_data <- read.csv(HIST_FILE, stringsAsFactors = FALSE); if(nrow(saved_data) > 0) history_data(saved_data) }) }
  
  current_level <- reactiveVal(1)
  session_score <- reactiveVal(0)
  session_lives <- reactiveVal(3)
  player_armor <- reactiveVal(0)
  
  player_weapon_tier <- reactiveVal(0)
  player_weapon_name <- reactiveVal("SHOTGUN")
  player_weapon_dmg <- reactiveVal(1)
  player_weapon_img <- reactiveVal(trouve_arme(0))
  player_flash_css <- reactiveVal("flash-default")
  
  monster_hp <- reactiveVal(1)
  monster_max_hp <- reactiveVal(1)
  session_monsters <- reactiveVal(character())
  current_loot_msg <- reactiveVal("")
  current_gore_anim <- reactiveVal("")
  
  current_question <- reactiveVal()
  game_phase <- reactiveVal("start") 
  last_explanation <- reactiveVal("")
  active_hint <- reactiveVal(FALSE)
  
  observeEvent(input$nav, {
    bg_img <- pioche_monstre(mboss_f)
    if(bg_img != "") {
      output$dynamic_bg <- renderUI({
        tags$style(HTML(paste0("body::before { content: ''; position: fixed; top: 0; left: 0; width: 100vw; height: 100vh; background-image: url('", bg_img, "'); background-size: cover; background-position: center; opacity: 0.15; z-index: -1; pointer-events: none; }")))
      })
    }
  })
  
  get_monster_max_hp <- function(lvl) {
    if(lvl <= 5) return(1)
    if(lvl >= 11 && lvl <= 19) return(3) 
    if(lvl == 10) return(4)
    if(lvl == 20) return(10)
    return(2)
  }
  
  generate_monsters_for_session <- function() {
    monsters <- character(20)
    for(i in 1:20) {
      if(i == 10) { monsters[i] <- pioche_monstre(mboss_f)
      } else if (i == 20) { monsters[i] <- pioche_monstre(fboss_f)
      } else if (i <= 5) { monsters[i] <- pioche_monstre(weak_f)
      } else if (i >= 11 && i <= 19) { monsters[i] <- pioche_monstre(strong_f)
      } else { monsters[i] <- pioche_monstre(med_f) }
    }
    return(monsters)
  }
  
  groupe_patho <- function(p) {
    if(grepl("CML|PV|Essential Thrombocythemia|Primary Myelofibrosis|Mastocytosis|Neutrophilic|Eosinophilic", p)) return("MYELOPROLIFERATIVE NEOPLASM")
    if(grepl("MDS|CMML", p)) return("MYELODYSPLASTIC SYNDROME")
    if(grepl("AML|BPDCN|MPDCP|MPAL|Undifferentiated|ETP", p)) return("ACUTE MYELOID / MIXED PHENOTYPE LEUKEMIA")
    if(grepl("ALL", p)) return("ACUTE LYMPHOBLASTIC LEUKEMIA")
    if(grepl("Lymphoma|CLL|Burkitt|Hairy Cell|Waldenström|Myeloma|Sézary|ATLL|AITL|LGLL|NK|MGUS|Amyloidosis", p)) return("MATURE LYMPHOID NEOPLASM")
    return("OTHER")
  }
  
  generate_single_question <- function(lvl) {
    asked <- asked_questions()
    noms_pathos <- names(pathologies)
    mots_interdits <- c("Non-specific", "Non-contributory karyotype", "Mostly normal karyotype", "Normal karyotype", "Complex anomalies", "Highly complex karyotype")
    
    max_attempts <- 100
    for(i in 1:max_attempts) {
      
      if (lvl <= 5) {
        choix_cat <- sample(c("morpho", "encadre"), 1, prob = c(0.85, 0.15))
      } else if (lvl >= 6 && lvl <= 9) {
        choix_cat <- sample(c("cd", "cyto", "encadre"), 1, prob = c(0.50, 0.40, 0.10))
      } else if (lvl == 10) {
        choix_cat <- sample(c("mut", "encadre"), 1, prob = c(0.85, 0.15))
      } else if (lvl >= 11 && lvl <= 19) {
        choix_cat <- sample(c("mut", "cd_cyto", "encadre"), 1, prob = c(0.70, 0.20, 0.10))
        if (choix_cat == "cd_cyto") choix_cat <- sample(c("cd", "cyto"), 1)
      } else {
        choix_cat <- sample(c("cyto", "mut", "cd", "clin", "morpho", "trait", "encadre"), 1)
      }
      
      if (choix_cat == "encadre") {
        nb_encadre <- sum(grepl("^ENC_", asked))
        if (nb_encadre < length(encadres_gris)) {
          idx <- sample(1:length(encadres_gris), 1)
          enc <- encadres_gris[[idx]]
          q_id <- paste0("ENC_", idx)
          
          if (!(q_id %in% asked)) {
            opts <- safe_sample(c(enc$a, safe_sample(enc$d, 4)), 5)
            cat_titre <- ifelse(grepl("\\[RED FRAME\\]", enc$q), "GENETICS / SCORE", "CLINICAL / DIAGNOSIS")
            asked_questions(c(asked, q_id))
            return(list(category = cat_titre, question = enc$q, options = opts, correct_ans = enc$a, explanation = enc$a))
          }
        }
      } else {
        type_q <- choix_cat
        patho <- safe_sample(noms_pathos, 1)
        
        # --- CORRECTION REDONDANCE (Généralisation) ---
        if (type_q == "cyto" && grepl("t\\(|inv\\(|del\\(|Ph\\+", patho, ignore.case = TRUE)) {
          type_q <- "mut"
        }
        if (type_q == "mut" && grepl("mutated", patho, ignore.case = TRUE)) {
          type_q <- "cd"
        }
        # ----------------------------------------------
        
        q_id <- paste("PATHO", patho, type_q, sep="_")
        
        if (!(q_id %in% asked)) {
          data_p <- pathologies[[patho]]
          bonne_rep <- data_p[[type_q]]
          
          if(!is.null(bonne_rep) && !(bonne_rep %in% mots_interdits)) {
            q_text <- paste(switch(type_q, "cyto"="CYTOGENETIC Abnormality", "mut"="MUTATION (Molecular Biology)", "cd"="IMMUNOPHENOTYPE (CD)", "clin"="CLINICAL Presentation", "morpho"="CYTOLOGY", "trait"="TREATMENT"), "of:", patho, "?")
            
            groupe_cible <- groupe_patho(patho)
            pathos_meme_groupe <- noms_pathos[sapply(noms_pathos, groupe_patho) == groupe_cible]
            
            fausses_reps <- unique(unlist(lapply(pathos_meme_groupe[pathos_meme_groupe != patho], function(p) {
              v <- pathologies[[p]][[type_q]]
              if(!is.null(v) && !(v %in% mots_interdits) && v != bonne_rep) return(v)
              return(NULL)
            })))
            
            if(length(fausses_reps) < 1) {
              fausses_reps <- unique(unlist(lapply(noms_pathos[noms_pathos != patho], function(p) {
                v <- pathologies[[p]][[type_q]]
                if(!is.null(v) && !(v %in% mots_interdits) && v != bonne_rep) return(v)
                return(NULL)
              })))
            }
            
            if(length(fausses_reps) > 4) fausses_reps <- safe_sample(fausses_reps, 4)
            
            opts <- sample(c(bonne_rep, fausses_reps))
            asked_questions(c(asked, q_id))
            
            cat_titre <- switch(type_q, "cyto"="CYTOGENETICS", "mut"="MOLECULAR", "cd"="IMMUNOPHENOTYPING", "clin"="CLINICAL", "morpho"="MORPHOLOGY", "trait"="TREATMENT")
            return(list(category = cat_titre, question = q_text, options = opts, correct_ans = bonne_rep, explanation = bonne_rep))
          }
        }
      }
    }
    
    enc <- encadres_gris[[1]]
    return(list(category = "GENETICS / SCORE", question = enc$q, options = c(enc$a, enc$d[1]), correct_ans = enc$a, explanation = enc$a))
  }
  
  start_session <- function() {
    asked_questions(character()) 
    session_monsters(generate_monsters_for_session())
    current_level(1)
    monster_max_hp(get_monster_max_hp(1))
    monster_hp(get_monster_max_hp(1))
    session_score(0); session_lives(3); player_armor(0)
    player_weapon_tier(0); player_weapon_name("SHOTGUN"); player_weapon_dmg(1)
    player_weapon_img(trouve_arme(0)); player_flash_css("flash-default")
    current_question(generate_single_question(1))
    game_phase("question")
  }
  
  observeEvent(input$btn_start_random, { start_session() })
  
  output$loot_panel <- renderUI({
    lvl <- current_level()
    p_mitra <- 0; p_laser <- 0; p_bazooka <- 0
    if(lvl >= 6 && lvl <= 10) p_mitra <- 5 + (lvl - 6) * 5
    if(lvl >= 11 && lvl <= 17) p_laser <- 5 + (lvl - 11) * 5
    if(lvl >= 18 && lvl <= 20) p_bazooka <- 15
    
    if (lvl <= 5) {
      q_probs <- "🔬 Cytology: 85%<br>🔲 Focus Boxes: 15%"
    } else if (lvl >= 6 && lvl <= 9) {
      q_probs <- "🩸 Phenotype: 50%<br>🧬 Cytogenetics: 40%<br>🔲 Focus Boxes: 10%"
    } else if (lvl == 10) {
      q_probs <- "🧬 Molecular: 85%<br>🔲 Focus Boxes: 15%"
    } else if (lvl >= 11 && lvl <= 19) {
      q_probs <- "🧬 Molecular: 70%<br>🩸 Pheno/Cyto: 20%<br>🔲 Focus Boxes: 10%"
    } else {
      q_probs <- "🎲 All categories: Balanced"
    }
    
    div(class="card", style="background: rgba(17,17,17,0.9); border: 2px solid #444; padding: 15px; margin-top: 15px;",
        h3("LOOT CHANCE", style="color:#f39c12; text-align:center; font-size:20px; padding:5px; margin-bottom: 15px; border-bottom: 2px dashed #444;"),
        p(sprintf("🔫 MACHINE GUN: %d%%", p_mitra), style=ifelse(p_mitra>0, "color:#18BC9C; font-weight:bold;", "color:#555;")),
        p(sprintf("🔦 BLUE LASER: %d%%", p_laser), style=ifelse(p_laser>0, "color:#3498db; font-weight:bold;", "color:#555;")),
        p(sprintf("🚀 BAZOOKA: %d%%", p_bazooka), style=ifelse(p_bazooka>0, "color:#E74C3C; font-weight:bold;", "color:#555;")),
        hr(style="border-color:#444; margin-top:15px; margin-bottom:15px;"),
        h3("QUESTION TYPE", style="color:#9b59b6; text-align:center; font-size:20px; padding:5px; margin-bottom: 15px; border-bottom: 2px dashed #444;"),
        p(HTML(q_probs), style="color:#eee; font-size:16px; text-align:center; line-height: 1.8;"),
        hr(style="border-color:#444; margin-top:15px; margin-bottom:15px;"),
        h3("GAME RULES", style="color:#18BC9C; text-align:center; font-size:20px; padding:5px; margin-bottom: 15px; border-bottom: 2px dashed #444;"),
        p("Welcome to HEMOCM fighter. Click on the correct answer and press FIRE to eliminate the monster at each level of the dungeon. Beware, the higher you climb, the stronger the monsters become. Random loots await to help you in this trial. Defeat the final boss at level 20 and win the game!", style="color:#eee; font-size:16px; text-align:justify;")
    )
  })
  
  output$game_ui <- renderUI({
    phase <- game_phase()
    if(phase == "start") return(div(class="card mt-3", style="background: rgba(34,34,34,0.9);", h2("MISSION: SURVIVAL"), actionButton("btn_start_random", "ENTER THE ARENA", class="btn-danger w-100 mt-3")))
    if(phase == "gameover") return(div(class="card mt-3", style="background: rgba(34,34,34,0.9);", h2("YOU DIED", style="color:red;"), actionButton("btn_start_random", "REPLAY", class="btn-danger w-100 mt-3")))
    if(phase == "victory") return(div(class="card mt-3", style="background: rgba(34,34,34,0.9);", h2("AREA CLEARED !"), actionButton("btn_start_random", "NEW MISSION", class="btn-success w-100 mt-3")))
    
    lvl <- current_level(); q <- current_question()
    lives_val <- max(0, session_lives()); hearts <- strrep("❤️", lives_val); if(lives_val < 3) hearts <- paste0(hearts, strrep("🖤", 3 - lives_val))
    if(player_armor() > 0) hearts <- paste0(hearts, " ", strrep("🛡️", player_armor()))
    hp_pct <- max(0, (monster_hp() / monster_max_hp()) * 100)
    
    hud_ui <- div(class="doom-hud",
                  div(class="hud-box", div(class="hud-label", "HEALTH"), div(class="hud-val", hearts)),
                  div(class="hud-box", div(class="hud-label", "WEAPON"), div(class="hud-val", paste0(player_weapon_name(), " (+", player_weapon_dmg(), ")"), style="color:#3498db;")),
                  div(class="hud-box", div(class="hud-label", "LEVEL"), div(class="hud-val", paste0(lvl, "/20"))),
                  div(class="hud-box", div(class="hud-label", "ENEMY HP"), div(class="hud-val", paste0(monster_hp(), "/", monster_max_hp()), style="color:red;"), div(class="monster-hp-bar", style=paste0("width:", hp_pct, "%;")))
    )
    
    current_monster_url <- session_monsters()[lvl]
    
    if(phase == "question") {
      is_new_monster <- (monster_hp() == monster_max_hp())
      anim_spawn_class <- ifelse(is_new_monster, "anim-spawn-seq", "")
      return(div(
        hud_ui,
        div(style = "min-height: 200px;",
            h3(q$question),
            div(class = "radio-container", { choix <- q$options; names(choix) <- paste("> ", choix); radioButtons("user_choice", label=NULL, choices=choix, selected=character(0)) })
        ),
        div(class="doom-viewport", 
            div(class=paste("monster-wrapper", anim_spawn_class), tags$img(src = current_monster_url, class = "gen-monster anim-breathing")),
            div(class=paste("muzzle-flash", player_flash_css())),
            div(class="weapon-bob-container", tags$img(src = player_weapon_img(), class = "gen-weapon")),
            actionButton("btn_submit", "FIRE", class = "btn-feu")
        )
      ))
    }
    
    if(grepl("feedback", phase)) {
      if(phase == "feedback_dead") { txt_color <- "#18BC9C"; titre <- "TARGET DESTROYED!"; msg <- "The monster is dead."; bg_anim <- "anim-flash-green"; monster_anim_class <- current_gore_anim(); action_class <- "action-shoot" }
      else if (phase == "feedback_hit") { txt_color <- "#f39c12"; titre <- "DIRECT HIT!"; msg <- "It is still alive!"; bg_anim <- ""; monster_anim_class <- ""; action_class <- "action-shoot action-hit" }
      else { txt_color <- "#E74C3C"; titre <- "DAMAGE TAKEN!"; msg <- paste("Explanation:", last_explanation()); bg_anim <- "anim-flash-red"; monster_anim_class <- ""; action_class <- "action-hurt" }
      
      div(hud_ui,
          div(class="card", style=paste("background: rgba(34,34,34,0.9); border-color:", txt_color, "; text-align:center; padding:10px; margin-bottom: 15px;"),
              h2(titre, style=paste("color:", txt_color, "; font-size: 16px;")), p(msg, style="line-height:2; font-size: 14px; font-weight:bold;")),
          div(class=paste("doom-viewport", bg_anim, action_class), 
              div(class=paste("monster-wrapper", monster_anim_class), tags$img(src = current_monster_url, class = "gen-monster")),
              div(class=paste("muzzle-flash", player_flash_css())),
              div(class="weapon-bob-container", tags$img(src = player_weapon_img(), class = "gen-weapon"))
          ),
          actionButton("btn_next", "PROCEED", class = "btn-primary w-100 mt-2")
      )
    }
  })
  
  observeEvent(input$btn_submit, {
    req(input$user_choice)
    q <- current_question()
    is_correct <- (input$user_choice == q$correct_ans)
    last_explanation(q$explanation)
    
    if(is_correct) {
      session_score(session_score() + 1)
      monster_hp(monster_hp() - player_weapon_dmg())
      
      if(monster_hp() <= 0) {
        current_gore_anim(ifelse(current_level() == 10 || current_level() == 20, "gore-explode", sample(c("gore-head", "gore-arm", "gore-explode"), 1)))
        lvl_next <- current_level() + 1
        if(lvl_next <= 20) {
          new_tier <- 0; new_name <- ""; new_dmg <- 1; new_img <- ""; new_flash <- ""
          if(lvl_next >= 6 && lvl_next <= 10 && runif(1) < ((5 + (lvl_next - 6) * 5) / 100)) { new_tier <- 1; new_name <- "MACHINE GUN"; new_dmg <- 2; new_img <- trouve_arme(1); new_flash <- "flash-machinegun" }
          else if(lvl_next >= 11 && lvl_next <= 17 && runif(1) < ((5 + (lvl_next - 11) * 5) / 100)) { new_tier <- 2; new_name <- "BLUE LASER"; new_dmg <- 3; new_img <- trouve_arme(2); new_flash <- "flash-laser" }
          else if(lvl_next >= 18 && lvl_next <= 20 && runif(1) < 0.15) { new_tier <- 3; new_name <- "BAZOOKA"; new_dmg <- 4; new_img <- trouve_arme(3); new_flash <- "flash-bazooka" }
          if(new_tier > player_weapon_tier()) { player_weapon_tier(new_tier); player_weapon_name(new_name); player_weapon_dmg(new_dmg); player_weapon_img(new_img); player_flash_css(new_flash) }
        }
        game_phase("feedback_dead")
      } else { game_phase("feedback_hit") }
    } else {
      dmg_taken <- ifelse(current_level() == 10 || current_level() == 20, 2, 1)
      if(player_armor() >= dmg_taken) player_armor(player_armor() - dmg_taken)
      else { session_lives(session_lives() - (dmg_taken - player_armor())); player_armor(0) }
      game_phase("feedback_hurt")
    }
    hist <- history_data(); hist <- rbind(hist, data.frame(id=current_level(), category=q$category, correct=is_correct, stringsAsFactors=FALSE)); history_data(hist)
  })
  
  observeEvent(input$btn_next, {
    if(session_lives() <= 0) game_phase("gameover")
    else if(game_phase() == "feedback_dead") {
      if(current_level() >= 20) game_phase("victory")
      else { lvl <- current_level() + 1; current_level(lvl); monster_max_hp(get_monster_max_hp(lvl)); monster_hp(monster_max_hp()); current_question(generate_single_question(lvl)); game_phase("question") }
    } else { current_question(generate_single_question(current_level())); game_phase("question") }
  })
  
  output$stats_plot <- renderPlot({
    hist <- history_data()
    if(nrow(hist)==0) return(NULL)
    stats <- hist %>% group_by(category) %>% summarize(taux = mean(correct)*100)
    ggplot(stats, aes(x=category, y=taux, fill=taux)) + geom_col() + theme_minimal() + theme(plot.background=element_rect(fill="transparent", color=NA), panel.background=element_rect(fill="transparent", color=NA), axis.text=element_text(color="white"))
  })
  
  output$weakness_recommendation <- renderUI({
    hist <- history_data()
    if(nrow(hist)<5) return(p("Insufficient data."))
    stats <- hist %>% group_by(category) %>% summarize(taux = mean(correct)) %>% arrange(taux)
    div(h2(stats$category[1]))
  })
  
  get_trophy_bg <- function(img_list) {
    img <- if(length(img_list)>0) sample(img_list, 1) else ""
    if(img == "") return("background: #222;")
    return(paste0("background-image: linear-gradient(rgba(0,0,0,0.6), rgba(0,0,0,0.8)), url('assets/", img, "'); background-size: cover; background-position: center; border-color: #E74C3C !important;"))
  }
  
  output$tr1 <- renderUI({ u <- nrow(history_data()) > 0; div(class=paste("trophy-card", ifelse(!u, "trophy-locked", "")), style=if(u) get_trophy_bg(weak_f), div(class="trophy-title", "ROOKIE"), div(class="trophy-desc", "Play your very first combat.")) })
  output$tr2 <- renderUI({ u <- sum(history_data()$correct) >= 50; div(class=paste("trophy-card", ifelse(!u, "trophy-locked", "")), style=if(u) get_trophy_bg(weak_f), div(class="trophy-title", "RBC"), div(class="trophy-desc", "Accumulate 50 correct answers.")) })
  output$tr3 <- renderUI({ u <- session_score() >= 10; div(class=paste("trophy-card", ifelse(!u, "trophy-locked", "")), style=if(u) get_trophy_bg(weak_f), div(class="trophy-title", "PLATELET"), div(class="trophy-desc", "Reach level 10 in survival.")) })
  output$tr4 <- renderUI({ u <- sum(history_data()$correct) >= 100; div(class=paste("trophy-card", ifelse(!u, "trophy-locked", "")), style=if(u) get_trophy_bg(med_f), div(class="trophy-title", "MEGAKARYOCYTE"), div(class="trophy-desc", "Accumulate 100 correct answers.")) })
  output$tr5 <- renderUI({ u <- session_score() >= 15; div(class=paste("trophy-card", ifelse(!u, "trophy-locked", "")), style=if(u) get_trophy_bg(med_f), div(class="trophy-title", "NEUTROPHIL"), div(class="trophy-desc", "Reach level 15 in survival.")) })
  output$tr6 <- renderUI({ u <- sum(history_data()$correct) >= 200; div(class=paste("trophy-card", ifelse(!u, "trophy-locked", "")), style=if(u) get_trophy_bg(med_f), div(class="trophy-title", "EOSINOPHIL"), div(class="trophy-desc", "Accumulate 200 correct answers.")) })
  output$tr7 <- renderUI({ u <- session_score() >= 18; div(class=paste("trophy-card", ifelse(!u, "trophy-locked", "")), style=if(u) get_trophy_bg(strong_f), div(class="trophy-title", "BASOPHIL"), div(class="trophy-desc", "Reach level 18 in survival.")) })
  output$tr8 <- renderUI({ u <- sum(history_data()$correct) >= 300; div(class=paste("trophy-card", ifelse(!u, "trophy-locked", "")), style=if(u) get_trophy_bg(strong_f), div(class="trophy-title", "MONOCYTE"), div(class="trophy-desc", "Accumulate 300 correct answers.")) })
  
  output$tr9 <- renderUI({ u <- session_score() == 20; div(class=paste("trophy-card", ifelse(!u, "trophy-locked", "")), style=if(u) get_trophy_bg(mboss_f), div(class="trophy-title", "MACROPHAGE"), div(class="trophy-desc", "Reach level 20 (Final Boss).")) })
  output$tr10 <- renderUI({ u <- sum(history_data()$correct) >= 500; div(class=paste("trophy-card", ifelse(!u, "trophy-locked", "")), style=if(u) get_trophy_bg(mboss_f), div(class="trophy-title", "BLAST"), div(class="trophy-desc", "Accumulate 500 correct answers.")) })
  output$tr11 <- renderUI({ u <- sum(history_data()$correct) >= 1000; div(class=paste("trophy-card", ifelse(!u, "trophy-locked", "")), style=if(u) get_trophy_bg(mboss_f), div(class="trophy-title", "STEM CELL"), div(class="trophy-desc", "Accumulate 1000 correct answers.")) })
  output$tr12 <- renderUI({ u <- sum(history_data()$correct) >= 1000 && session_score() >= 20; div(class=paste("trophy-card", ifelse(!u, "trophy-locked", "")), style=if(u) get_trophy_bg(mboss_f), div(class="trophy-title", "EXPERT"), div(class="trophy-desc", "Win the game with 1000 total correct answers.")) })
  
  output$tr13 <- renderUI({ u <- session_score() >= 1; div(class=paste("trophy-card", ifelse(!u, "trophy-locked", "")), style=if(u) get_trophy_bg(fboss_f), div(class="trophy-title", "FIRST BLOOD"), div(class="trophy-desc", "Defeat your first monster.")) })
  output$tr14 <- renderUI({ u <- current_level() > 10; div(class=paste("trophy-card", ifelse(!u, "trophy-locked", "")), style=if(u) get_trophy_bg(fboss_f), div(class="trophy-title", "GIANT KILLER"), div(class="trophy-desc", "Defeat the level 10 mini-boss.")) })
  output$tr15 <- renderUI({ u <- game_phase() == "victory"; div(class=paste("trophy-card", ifelse(!u, "trophy-locked", "")), style=if(u) get_trophy_bg(fboss_f), div(class="trophy-title", "DRAGON SLAYER"), div(class="trophy-desc", "Clear the game by killing the final boss.")) })
  output$tr16 <- renderUI({ u <- game_phase() == "victory" && session_lives() == 3; div(class=paste("trophy-card", ifelse(!u, "trophy-locked", "")), style=if(u) get_trophy_bg(fboss_f), div(class="trophy-title", "UNTOUCHABLE"), div(class="trophy-desc", "Clear the entire game without losing a single life.")) })
}

shinyApp(ui, server)