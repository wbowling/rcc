int lala(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k) {
    return a + b + c + d + e + f + g + h + i + j + k;
}

int main() {
    int a = lala(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11);
    int b = 0x0A6425;
    printf(&b, a);
    return a;
}