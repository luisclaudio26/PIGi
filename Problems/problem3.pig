# Problema 3:
# Crie um programa que leia duas matrizes numéricas e, quando possível, imprima
# a soma e o produto dessas matrizes. Caso uma operação não possa ser realizada
# para as matrizes lidas, imprima uma mensagem informando da impossibilidade.

module problem3;

proc printMat(A: mat, r, c: int) {
	def i, j: int;
	i, j = 0, 0;
	while i < r {
        j = 0;
		while j < c {
			print(A[i,j]);
			print(" | ");
			j = j + 1;
		}
		i = i + 1;
		println("");
	}
}

proc main() {
	println("PROBLEM 3:");
	def m1, m2 : mat;
	def rows1, cols1, rows2, cols2 : int;

	# leia dimensão da primeira matriz
	println("Number of rows of first matrix:");
	rows1 = readInt();
	println("Number of columns of first matrix:");
	cols1 = readInt();
 	# println(rows1);
	# println(cols1);
	m1 = zeros(rows1, cols1);
	
	# leia primeira matriz
	def i, j : int;
	i, j = 0, 0;
	
	while i < rows1 {
        j = 0;
		while j < cols1 {
			m1[i,j] = readFloat();
			j = j + 1;
		}
		i = i + 1;
	}

	# leia dimensão da segunda matriz
	println("Number of rows of second matrix:");
	rows2 = readInt();
	println("Number of columns of second matrix:");
	cols2 = readInt();

	m2 = zeros(rows2, cols2);

	# leia segunda matriz
	
	i, j = 0, 0;
	while i < rows2 {
        j = 0;
		while j < cols2 {
			m2[i,j] = readFloat();
			j = j + 1;
		}
		i = i + 1;
	}

    # somar matrizes
    if (cols1 == cols2 and rows1 == rows2) {
        def sum: mat;
        sum = m1 + m2;
        printMat(sum, rows1, cols1);

    }
    else {
        println("invalid sum dimensions");
    }
	println("");
    #multiplicar matrizes
    if (cols1 == rows2) {
        def mult : mat;
        mult = m1 * m2;
        printMat(mult, rows1, cols2);
    }
    else {
        println("invalid multiplication dimensions");
    }
}
