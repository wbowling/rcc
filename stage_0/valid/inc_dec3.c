int main() {
    int a = 2;
    int b = a++;
    int c = ++a;
    b = b++ + ++c + a;

    return a++ + --b + c--;
}