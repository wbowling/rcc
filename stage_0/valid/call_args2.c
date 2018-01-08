int lala2(int aaa) {
    int bbb = 9;
    bbb = aaa * bbb;
    return aaa * bbb;
}

int lala1(int a, int b, int c) {
    return a * b + 5;
}

int main() {
    return lala2(1) + lala1(1, 2, 3);
}