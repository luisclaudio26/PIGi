module problem2;

#Crie um programa que leia uma quantidade desconhecida de números e informe quantos deles estão nos seguintes intervalos fechados: [0, 25], [26, 50], [51, 75] e [76, 100]. A entrada de dados deve terminar quando for lido um número negativo.

proc main ()
{
	def w, x, y, z : int;
	w, x, y, z = 0,0,0,0;
	def number : int;
	println("PROBLEM 2: ");
	number = readInt();
	
	while (number >= 0) {
		if number < 26 {
			w = w+1;
		}
		else if number < 51 {
			x = x+1;
		}
		else if number < 76 {
			y = y+1;
		}
		else if number < 101 {
			z = z+1;
		}

		number = readInt();
	}
	
	println( "Numbers in range: [0, 25]   = " + toString(w) );
	println( "Numbers in range: [26, 50]  = " + toString(x) );
    println( "Numbers in range: [51, 75]  = " + toString(y) );
    println( "Numbers in range: [76, 100] = " + toString(z) );
}