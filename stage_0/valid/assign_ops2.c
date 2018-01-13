int main() {
    int a = 2;
    a ^= 7;
    int b = 7;
    a &=8;
    b |= 11;
    int c = a + b;
    a <<= c + b;
    b >>= 2;
    return b;
}