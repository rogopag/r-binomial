# r-binomial
Simple R (RC) Class to collect Pascal's Arithmetic Triangle "consequences".

Every instance of the class represents a single Triangle as a low triangular matrix.

We didn't follow Blaise Pascal construction rules (*Triangulus Arithmeticus, I-Definitiones* in Blaise Pascal, *Opere Complete*, 2020 Giunti Milano), instead we followed the algorythm presented in Gilbert Strang, *Introduction To Linear Algebra*, 2.4 A p. 72:

      Lij + Lij-1 = Li+1j, 

which derivesdireclty from Consect. 8 in Pascal treatise: *"Summa cellularum basis (i) cujuslibet unitate minuta (i-1) aequatur summae cellularum basium onnium praecedentiumHoc enim est proprium progressionis duplae quae ab unitate incipit, ut quilibet ejus numerus, unitate minutus, aequatur omnium praecedentium."*, in equation form:

      (Lij) - 1 = sum(L[i], i==1, i-1).

From the 2 preceding equations it is easy to derive the following:

      Lij = Li-1j + Li-1j-1 

which is the usual simplified form we used in our main method pascalBinomial().
