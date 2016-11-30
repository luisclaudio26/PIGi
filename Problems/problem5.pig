module problem5;

#Crie um subprograma chamado mdc, com três argumentos n, m (passados por valor) e r (passado por referência), nesta ordem. 
# O subprograma mdc deve calcular o maior divisor comum entre dois números naturais estritamente positivos n e m, de acordo com o seguinte algoritmo recursivo:
## Se n for um divisor de m, n é o maior divisor comum de n e m.
## Se m for um divisor de n, m é o maior divisor comum de n e m.
## Se n não for um divisor de m, e se m for maior que n, então o maior divisor comum de m e n é também o maior divisor comum de n e do resto da divisão de m por n.
# O subprograma deve retornar seu resultado por meio de parâmetro r, que deve ser posteriormente impresso na tela pelo programa principal.

proc mdc(n, m: int, r: mut ref int){
def a : int;
	if m<0 or n < 0 { r = -1;}
	else{
		if (m mod n) == 0 { r = n; } 
		else if (n mod m) == 0 { r = m; }
		else if m>n {
			mdc ( n,(m mod n), r) ;
		}
	}
}

proc main(){
	println("PROBLEM 5:");
	def m, n, r: int;
	r = -1;
	
	println("first number:");
	n = readInt();

	println("second number:");
	m = readInt();
	
	mdc(n, m, r);

	if r =/= -1 {
		println ("O mdc é " + toString(r));
	} else {
		println ("error");
	}
}