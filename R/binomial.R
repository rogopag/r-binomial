# binomial
# install.packages(pkgs='plot.matrix')
# library('plot.matrix')

'Every instance of the class represents a single Triangle as a low triangular matrix.
We didn\'t follow Blaise Pascal construction rules (Triangulus Arithmeticus, I-Definitiones in Blaise Pascal, Opere Complete, 2020 Giunti Milano),
instead we followed the algorythm presented in Gilbert Strang, Introduction To Linear Algebra, 2.4 A p. 72, as Lij + Lij-1 = Li+1j, which derives
direclty from Consect. 8 in Pascal treatise: "Summa cellularum basis (i) cujuslibet unitate minuta (i-1) aequatur summae cellularum basium onnium praecedentium
Hoc enim est proprium progressionis duplae quae ab unitate incipit, ut quilibet ejus numerus, unitate minutus, aequatur omnium praecedentium.", in equation form
(Lij) - 1 = sum(L[i], i==1, i-1).

From the 2 preceding equations it is easy to derive the following: Lij = Li-1j + Li-1j-1 which is the usual simplified form we used in our main method pascalBinomial().
'

binomial <- setRefClass(
  'binomial',
  fields =
    list(k = 'numeric', a2 = 'matrix'),
  methods =
    list(
    initialize = function(k = 0) {
      k ->> k
      tryCatch({
        pascalBinomial() ->> a2
      }, error = function(e) {
        print(e)
        return(0)
      })
    },
    pascalBinomial = function() {
      'Implements Lij = Li-1j + Li-1j-1 algorythm'
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
    pascalBinomialAndSquaresCol = function() {
      a2 -> a
      oneVectorMatrix() -> v
      a %*% v -> d
      return(cbind(a, d))
    },
    computeRowsSum = function(a) {
      return(apply(a, 1, sum))
    },
    addComputedSquaresColumn = function() {
      a2 -> a
      computeRowsSum(a) -> s
      return(cbind(a, s))
    },
    computePascal3DMatrix = function() {
      a2 -> a
      a %*% a -> a3
      return(a3)
    },
    computeCubeMatrixAndThreeSquaresCol = function() {
      computePascal3DMatrix() -> a3
      oneVectorMatrix() -> one
      a3 %*% one -> a3d
      return(cbind(a3, a3d))
    },
    plot = function(matrix) {
      matrix -> pascal_binomial
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
    }
  )
)
