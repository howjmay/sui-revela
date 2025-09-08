// Testcase 8: `while`
module 0x23::tc8 {
    public fun foo(): u64 {
        let mut a = 0;
        let mut i = a;
        while (i < 5) {
            i = i + 1;
            a = a - i;
        };
        i = i + 2;
        return i + a
    }
}
