module problem4;

struct rational_t = (num, den: int);

#Subprograma que, dados dois parÃ¢metros inteiros a e b, onde b != 0, retorna um valor rational_t para representar a fraÃ§Ã£o a=b.
func r : rational_t = createRational(a, b: int){
    if b =/= 0 {
        def x, y: int;
        x, y = simp(a, b);
        if y < 0 {
            x, y = -x, -y;
        }
        r = rational_t(x, y);
    }
    else{
        println("den is equal 0");
    }
}

# Subprograma que, dados dois parÃ¢metros do tipo rational_t, retorna true se eles representam o mesmo nÃºmero racional ou false, em caso contrÃ¡rio.
func r : bool = isEqual(a, b: rational_t) {
    r = (a->num * b->den == a->den * b->num);
}

# Subprograma que retorna um valor rational_t correspondente a soma entre dois valores rational_t passados como parÃ¢metros.
func r: rational_t = sum(a, b: rational_t) {
    if(a->den =/= 0 or b->den =/= 0){
        def n, d: int;
        d = a->den * b->den;
        n = a->num * b->den + b->num * a->den;
        r = createRational(n, d);
    } else {
        println("den is equal 0");
        r = rational_t (1,1);
    }
}

# Subprograma que retorna um valor rational_t correspondente a negaÃ§Ã£o de um valor rational_t passado como parÃ¢metro.
func r: rational_t = neg(a: rational_t) {
    r = rational_t(-1 * a->num, a->den);
}

# Subprograma que retorna um valor rational_t correspondente a subtraÃ§Ã£o entre dois valores rational_t passados como parÃ¢metros.
func r: rational_t = sub(a, b: rational_t){
    r = sum(a, neg(b));
}

# Subprograma que retorna um valor rational_t correspondente a multiplicaÃ§Ã£o entre dois valores rational_t passados como parÃ¢metros.
func r: rational_t = mult(a, b: rational_t){

    def n, d: int;
    r = rational_t(0,0);
    if(a->den =/= 0 or b->den =/= 0){
        n = a->num * b->num;
        d = a->den * b->den;
        r = createRational(n, d);
    } else{
        print("den is equal 0");
        r = rational_t (1,1);
    }
}

# Subprograma que retorna um valor rational_t correspondente ao inverso de um valor rational_t passado como parÃ¢metros.
func r: rational_t = inv(a: rational_t){

    if(a->den =/= 0) { 
        r = rational_t(a->den, a->num);
    }
    else {
        print("den is equal 0");
        r = rational_t (1,1);
    }
}

# Subprograma que retorna um valor rational_t correspondente a divisÃ£o entre dois valores rational_t passados como parÃ¢metros.
func r: rational_t = div(a, b: rational_t) {
    r = mult(a, inv(b));
}


func x, y: int = simp(a, b: int) {
    def m: int;
    m = gcd(a, b);
    x, y = a/m, b/m;
}


func m: int = gcd(a, b: int) {
    def x, y: int;

    if a > b {
        x, y = a, b;
    }
    else {
        x, y = b, a;
    }

    while y =/= 0 {
        x, y = y, x mod y;
    }
    m = x;
}

func m: int = mmc(a, b: int){
    def x, y: int;

    if a > b {
        x, y = a, b;
    }
    else {
        x, y = b, a;
    }

    while y =/= 0 {
        x, y = y, x mod y;
    }

    m = (a*b) / x;
}

func s: string = ratToStr(r: rational_t) {
    s = toString(r->num) + "/" + toString(r->den);
}

proc printp4 (number1, number2 : rational_t) {
    def r : rational_t;
	r = sum(number1, number2);
    println("Sum: " + ratToStr(r)); 

    r = neg(number1);
    println("Neg of number 1: " + ratToStr(r));
    r = neg(number2);
    println("Neg of number 2: " + ratToStr(r));

    r = sub(number1, number2);
    println("Sub: " + ratToStr(r));

    r = mult(number1, number2);
    println("Mult: " + ratToStr(r));

    r = inv(number1);
    println("Inv of first number: " + ratToStr(r));
    r = inv(number2);
    println("Inv of second number: " + ratToStr(r));

    r = div(number1, number2);
    println("Div: " + ratToStr(r));
}

proc main(){
    def number1, number2 : rational_t;
    def aux1, aux2: int;

    println("Write the num of the first number:");
    aux1 = readInt(); 
    println ("Write the den of the first number(not equal 0):");
    aux2 = readInt();
    number1 = createRational(aux1, aux2);

    println ("Write the num of the second number:");
    aux1 = readInt();

    println ("Write the den of the first number(not equal 0):");
    aux2 = readInt();
    number2 = createRational(aux1, aux2);

    if isEqual(number1, number2){
        println("They represent the same number");
    } else {
        println("They don't represent the same number");
    }

    printp4(number1, number2);
}