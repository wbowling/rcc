int lala1() {
    return 5 * 2;
}

int lala2() {
    int a = 7;
    return 9 * a;
}


int main() {
    return lala1(1, 2, 3) + lala2();
}