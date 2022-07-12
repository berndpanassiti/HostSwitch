#' Consumer life cycle parameters to simulate host switches
#'
#' Data derived from real world experiments (see sources). Data includes
#' life cycle parameters for a wildlife ecology, agricultural
#' and biomedical arena  to simulate host switches.\cr
#' Semantics of parli = \bold{par}ameter of \bold{li}fe cycle
#'
#' @docType data
#'
#' @usage data(parli)
#'
#' @format An object of class "list". The list contains three matrices:
#' $Cephaloleia, $Cacopsylla, and $SarsMers.
#'\enumerate{
#' \item{\emph{parli$Cephaloleia} is a data frame with 11 rows (parameters)
#'       and 8 columns (variables).}
#'
#' The parameters are classified in three groups:
#'\itemize{
#'   \item{Arbitrary varying: mig (probability of migration) and jump_back
#'         (to the original Resource).}
#'   \item{Arbitrary fixed: K (carrying capacity), N-generations, seed,
#'         n_sim, pRes_min, pRes_max, sd (standard deviation of mutation),
#'         and sigma (standard deviation of survival).}
#'   \item{From real data: b (net reproduction rate) (Garcia-Robledo and
#'         Horvitz, 2011).}
#'}
#'  The column name acronyms are assigned after:
#'\itemize{
#'   \item{two \emph{Cephaloleia} species: \bold{Cb} for \emph{C. belti} and
#'         \bold{Cp} for \emph{C. placida}.}
#'   \item{two levels of variation for mig: \bold{mL} for low and \bold{mH}
#'         for high migration.}
#'   \item{two levels for jump_back: \bold{jY} for jumping back and \bold{jN}
#'         for no jumping back individuals.}
#'}
#'
#' \item{\emph{parli$Cacopsylla} is a data frame with 11 rows and 8 columns.}
#' The parameters are classified in three groups:
#'\itemize{
#'   \item{Arbitrary varying: sd and jump_back.}
#'   \item{Arbitrary fixed: K, N-generations, seed, n_sim, pRes_min, pRes_max}
#'   \item{From real data: b, sigma and mig (Malagnini et al. 2010, 2011;
#'         Mayer et al. 2013).}
#'}
#' The column name acronyms are assigned after:
#'\itemize{
#'   \item{two \emph{Cacopsylla melanoneura} populations: \bold{CmA} for
#'         \emph{C. melanoneura} adapted to Apple and \bold{CmH} for
#'         \emph{C. melanoneura} adapted to hawthorn.}
#'   \item{two levels of variation for sd: \bold{sdL} for low and \bold{sdH}
#'        for high standard deviation of mutation.}
#'   \item{two levels for jump_back: \bold{jY} for jumping
#'         back and \bold{jN} for no jumping back individuals.}
#'}
#'
#' \item{\emph{parli$SarsMers}is a data frame with 11 rows and 8 columns.}
#' The parameters are classified in three groups:
#'\itemize{
#'   \item{Arbitrary varying: mig}
#'   \item{Arbitrary fixed: K, N-generations, seed, n_sim, pRes_min, pRes_max}
#'   \item{From real data: b, sigma, and sd (Kim et al. 2021; van Dorp et al. 2020)}
#'}
#' The column name acronyms are assigned after:
#'\itemize{
#'   \item{two coronaviruses: \bold{Sars} for \emph{Sarbecovirus} sp. and
#'         \bold{Mers} for \emph{Merbacovirus} sp.}
#'   \item{three levels of variation for mig: \bold{migL} for low, \bold{migM}
#'         for medium and \bold{migH} for high probability of migration.}
#'}
#' }
#'
#' @keywords datasets
#' @source Garcia-Robledo C and Horvitz CC (2011) Experimental
#' demography and the vital rates of generalist and specialist
#' insect herbivores on native and novel host plants.
#' Journal of Animal Ecology, 80(5):976-989.
#'
#' Kim KS, Ejima K, Iwanami S, Fujita Y, Ohashi H, Koizumi Y,
#' Asai Y, Nakaoka S, Watashi K, Aihara K, et al. (2021)
#' A quantitative model used to compare within-host sars-cov-2,
#' mers-cov, and sars-cov dynamics provides insights into the
#' pathogenesis and treatment of sars-cov-2. PLoS biology,
#' 19(3):e3001128.
#'
#' Malagnini V, Pedrazzoli F, Gualandri V, Forno F, Zasso R,
#' Pozzebon A, and Ioriatti C. (2010) A study of the effects
#' of 'candidatus phytoplasma mali' on the psyllid Cacopsylla
#' melanoneura (Hemiptera: Psyllidae). Journal of invertebrate
#' pathology, 103(1):65-67.
#'
#' Malagnini V, Pedrazzoli F, Papetti C, Cainelli C, Zasso R,
#' Gualandri V, Pozzebon A, and Ioriatti C. (2013) Ecological
#' and genetic differences between Cacopsylla melanoneura
#' (Hemiptera, Psyllidae) populations reveal species host
#' plant preference. PloS one, 8(7):e69663.
#'
#' Mayer CJ, Vilcinskas A, and Gross J. (2011) Chemically mediated
#' multitrophic interactions in a plant-insect vector-phytoplasma
#' system compared with a partially nonvector species. Agricultural
#' and Forest Entomology, 13(1):25-35.
#'
#' van Dorp L, Acman M, Richard D, Shaw LP, Ford CE, Ormond L, Owen CJ,
#' Pang J, Tan CC, Boshier FA, et al (2020) Emergence of genomic diversity
#' and recurrent mutations in sars-cov-2. Infection, Genetics and
#' Evolution, 83:104351.
#'
#' @references Trivellone V, Araujo SBL and Panassiti B
#' (2021) HostSwitch: An R Package to Simulate
#' the Extent of Host-Switching by a Consumer (submitted).
#'
#' @examples
#' data(parli)
#' knitr::kable(parli$Cephaloleia) # tibble::as.tibble(parli$Cephaloleia)
#'
"parli"
