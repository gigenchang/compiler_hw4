int main() {
	int i;
        int p;
        int k, j;
		int p; /*重複宣告*/
	for (i=0; i<9; i=i+1) {
            /* empty */
	}
        for (i=0, p=4+4; i<9; i=i+1, p=i) {
            write("test");
        }
        for (;;) {
            /*empty*/
        }
        while (1) {
			int p;   /*不算重複宣告*/
			int a = 3, b, c;
			int a;   /*重複宣告*/
        }
        while (2) {
            write("haha");
            write("wow");
			z = 100;     /*左值未宣告*/
			p = z + 3;   /*右值未宣告*/
			zz("what the hell");  /*未宣告的function call*/
			p = 3 * z + 3;   /*更複查一點的右值未宣告*/
			z = 3 + z;   /*左值又值都未宣告*/
        }
        if (k == j) {
            write("first");
        }
        if (k == j) {
            write("first");
        } else if (j == k) {
            write("second");
        } else {
            write("third");
        }
}
