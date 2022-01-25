################################################################################
#
#'
#' Create S3M-SQUEAC diagram using DiagrammeR
#'
#
################################################################################

draw_s3m_squeac_graph <- function() {
  DiagrammeR::mermaid("
    graph TD
    A(Start)                  --> B[S3M Suveys]
    B[S3M Suveys]             --> C{Coverage OK?}
    C{Coverage OK?}           --> D[YES]
    C{Coverage OK?}           --> E[NO]
    D[YES]                    --> F[SQUEAC investigations]
    E[NO]                     --> G[SQUEAC investigations]
    F[SQUEAC investigations]  --> H((Compare and contrast))
    G[SQUEAC investigations]  --> H((Compare and contrast))
    H((Compare and contrast)) --> I[Reform programme]
    I[Reform programme]       --> B[S3M Surveys]
  ")
}