module problem1;

# Crie um programa que, dados três valores numéricos x, y e c, onde x e y são números racionais e c é um número inteiro, previamente armazenados no código fonte, avalia a expressão x^2 - y + c e imprime seu resultado na tela.

proc main ()
{
	def x, y : float;
	def c: int;
	x = 5.0;
	y = 4.2;
	c = 3;

	println("PROBLEM 1: ");
	println("x^2 - y + c ");
	println("=("+ toString(x) + ")^2 -"+ toString(y) + "+" + toString(c));
	println("=" + toString(problem1(x, y, c)));
}

func resp: float = problem1(x, y: float, c: int){
	resp = x^toFloat(2) - y + toFloat(c);
}