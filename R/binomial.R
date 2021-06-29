# binomial
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#install.packages(pkgs='plot.matrix')
#library('plot.matrix')

binomial <- setRefClass(
  'binomial',
  fields =
    list(k = 'numeric'),
  methods =
    list(
    pascalBinomial = function() {
      if (k < 0) {
        stop(
          sprintf(
            'argument k should be 0 or greater (positive integer), %.d given instead',
            k
          ),
          call. = TRUE
        )
      }

      k + 1 -> n

      ifelse(k > 0, n, 2) -> col

      matrix(0,
             nrow = n,
             ncol = col,
             byrow = TRUE) -> mat

      for (i in 1:n) {
        c() -> a
        i - 1 -> exp
        for (j in 1:col) {
          j - 1 -> prev
          0 -> x
          if (j == 1 || j == i) {
            1 -> x
          } else if (j > i) {
            0 -> x
          } else if (j < i) {
            mat[exp, j] +  mat[exp, prev] -> x
          }

          c(a, x) -> a
        }

        a -> mat[i, ]
      }
      return(mat)
    },
    oneVectorMatrix = function() {
      k + 1 -> n
      matrix(0,
             nrow = 1,
             ncol = n,
             byrow = TRUE) -> mat

      c() -> a

      for (i in 1:n) {
        c(a, 1) -> a
      }

      a -> mat[1, ]

      return(a)
    },
    pascalBinomialAndTwoSquaresCol = function() {
      pascalBinomial() -> a
      oneVectorMatrix() -> v
      a %*% v -> d
      return(cbind(a, d))
    },
    computeRowsSum = function(a) {
      return(apply(a, 1, sum))
    },
    addRowsSumAsIndependentCol = function() {
      tryCatch({
        pascalBinomial() -> a
      }, error = function(e) {
        print(e)
        return(0)
      })

      computeRowsSum(a) -> s
      return(cbind(a, s))
    },
    plotPascalBinomial = function(matrix) {
      pascal_binomial <- matrix
      class(pascal_binomial)
      par(mar = c(5.1, 4.1, 4.1, 4.1)) # adapt margins
      plot(
        pascal_binomial,
        breaks = c(1:sqrt(length(matrix))),
        key = NULL,
        fmt.cell = '%.0f',
        axis.row = NULL,
        axis.col = NULL
      )
    },
    computePascal3DMatrix = function() {
      a <- pascalBinomial()
      a3 <- a %*% a
      return(a3)
    },
    computeCubeMatrixAndThreeSquaresCol = function() {
      a3 <- computePascal3DMatrix()
      one <- oneVectorMatrix()
      a3d <- a3 %*% one
      return(cbind(a3, a3d))
    }
  )
)
