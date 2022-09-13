#pragma GCC optimization ("O2")
#include <string>
#include <algorithm>
#include <deque>
#include <fstream>
#include <vector>
#include <ctime>
#include <iostream>
#include <sstream>

using namespace std;


const long long MOD[] = { 999999990047, 999999990499, 999999990701, 999999991043, 999999991297, 999999991547, 999999991867, 999999992141, 999999992389, 999999992737, 999999993079, 999999993611, 999999993907, 999999994361, 999999994517, 999999994781, 999999994999, 999999995177, 999999995461, 999999995857 };


class bignum {
private:
    static const int base = 1000000000;
    static const int base_digits = 9;
    int sign;
    vector<int> a;

    void trim() {
        while (!a.empty() && a.back() == 0) {
            a.pop_back();
        }
        if (a.empty()) {
            sign = 1;
        }
    }

    bool Is_Zero() const {
        return (a.empty() || (a.size() == 1 && a[0] == 0));
    }

    void read(const string& s) {
        sign = 1;
        a.clear();
        int pos = 0;
        while (pos < (int)s.size() && (s[pos] == '-' || s[pos] == '+')) {
            if (s[pos] == '-') {
                sign = -sign;
            }
            ++pos;
        }
        for (int i = s.size() - 1; i >= pos; i -= base_digits) {
            int x = 0;
            for (int j = max(pos, i - base_digits + 1); j <= i; j++) {
                x *= 10;
                x += s[j] - '0';
            }
            a.push_back(x);
        }
        trim();
    }

    string int_to_string(int x) {
        string res = "";
        if (x == 0) {
            return "0";
        }
        while (x != 0) {
            res += x % 10 + '0';
            x /= 10;
        }
        reverse(res.begin(), res.end());
        return res;
    }
public:
    bignum abs() const {
        bignum res = *this;
        res.sign = 1;
        return res;
    }

    bignum operator - () const {
        bignum res = *this;
        res.sign = -sign;
        return res;
    }

    bignum() : sign(1) {}

    string to_string(int len = 0) {
        if (a.empty()) {
            return "0";
        }
        string res = int_to_string(a.back());
        for (int i = (int)a.size() - 2; i >= 0; i--) {
            string tmp = int_to_string(a[i]);
            for (int j = 0; j < base_digits - tmp.size(); j++) {
                res += '0';
            }
            res += tmp;
        }

        //fill to len
        if (len != 0) {
            string tmp = "";
            for (int i = 0; i < len - res.size(); i++) {
                tmp += '0';
            }
            res = tmp + res;
        }

        return res;
    }

    bignum& operator = (long long A) {
        sign = 1;
        a.clear();
        if (A < 0) {
            sign = -1;
            A = -A;
        }
        for (; A > 0; A /= base) {
            a.push_back(A % base);
        }
        return *this;
    }

    bignum& operator = (int A) {
        sign = 1;
        a.clear();
        if (A < 0) {
            sign = -1;
            A = -A;
        }
        for (; A > 0; A /= base) {
            a.push_back(A % base);
        }
        return *this;
    }

    bignum& operator = (string s) {
        sign = 1;
        a.clear();
        int pos = 0;
        while (pos < (int)s.size() && (s[pos] == '-' || s[pos] == '+')) {
            if (s[pos] == '-') {
                sign = -sign;
            }
            ++pos;
        }
        for (int i = s.size() - 1; i >= pos; i -= base_digits) {
            int x = 0;
            for (int j = max(pos, i - base_digits + 1); j <= i; j++) {
                x *= 10;
                x += s[j] - '0';
            }
            a.push_back(x);
        }
        trim();
        return *this;
    }

    bignum(long long A) {
        *this = A;
    }

    bignum(int A) {
        *this = (long long)A;
    }

    bignum(string s) {
        *this = s;
    }

    bignum(int base, int exponent) {
        bignum res = 1;
        bignum A = base;
        for (; exponent > 0; exponent >>= 1) {
            if (exponent & 1) {
                res *= A;
            }
            A = A * A;
        }
        *this = res;
    }

    bignum& operator = (const bignum& A) {
        sign = A.sign;
        a = A.a;
        return *this;
    }

    bool operator < (const bignum& A) const {
        if (sign != A.sign) {
            return sign < A.sign;
        }
        if (a.size() != A.a.size()) {
            return sign * a.size() < sign * A.a.size();
        }
        for (int i = a.size() - 1; i >= 0; i--) {
            if (a[i] != A.a[i]) {
                return sign * a[i] < sign * A.a[i];
            }
        }
        return 0;
    }

    bool operator > (const bignum& A) const {
        return A < *this;
    }

    friend bool operator >= (const bignum& A, const bignum& B) { return !(A < B); }
    friend bool operator <= (const bignum& A, const bignum& B) { return !(A > B); }
    friend bool operator == (const bignum& A, const bignum& B) { return !(A < B) && !(A > B); }

    bignum operator + (const bignum& A) const {
        if (sign == A.sign) {
            bool carry = 0;
            bignum res = A;
            for (int i = 0; i < (int)max(a.size(), A.a.size()) || carry; i++) {
                if (i == (int)res.a.size()) {
                    res.a.push_back(0);
                }
                res.a[i] += carry + (i < (int)a.size() ? a[i] : 0);
                carry = (res.a[i] >= base);
                if (carry) {
                    res.a[i] -= base;
                }
            }
            return res;
        }
        return *this - (-A);
    }

    bignum& operator += (const bignum& A) { *this = *this + A; return *this; }

    bignum operator - (const bignum& A) const {
        if (sign == A.sign) {
            if (abs() >= A.abs()) {
                bignum res = *this;
                bool carry = 0;
                for (int i = 0; i < (int)A.a.size() || carry; i++) {
                    res.a[i] -= carry + (i < (int)A.a.size() ? A.a[i] : 0);
                    carry = (res.a[i] < 0);
                    if (carry) {
                        res.a[i] += base;
                    }
                }
                res.trim();
                return res;
            }
            return -(A - *this);
        }
        return *this + (-A);
    }

    bignum& operator -= (const bignum& A) { *this = *this - A; return *this; }

    bignum& operator *= (int A) {
        if (A < 0) {
            sign = -sign;
            A = -A;
        }
        int carry = 0;
        for (int i = 0; i < (int)a.size() || carry; i++) {
            if (i == (int)a.size()) {
                a.push_back(0);
            }
            long long cur = a[i] * (long long)A + carry;
            carry = cur / base;
            a[i] = cur % base;
        }
        trim();
        return *this;
    }

    bignum& operator *= (long long A) {
        if (A < 0) {
            sign = -sign;
            A = -A;
        }
        if (A > base) {
            *this = *this * (A / base) * base + *this * (A % base);
            return *this;
        }
        int carry = 0;
        for (int i = 0; i < (int)a.size() || carry; i++) {
            if (i == (int)a.size()) {
                a.push_back(0);
            }
            long long cur = 1LL * a[i] * A + carry;
            carry = cur / base;
            a[i] = cur % base;
        }
        trim();
        return *this;
    }

    bignum operator * (int A) { bignum res = *this; res *= A; return res; }
    bignum operator * (long long A) { bignum res = *this; res *= A; return res; }

    static vector<int> convert_base(const vector<int>& a, int old_digits, int new_digits) {
        vector<long long> p(max(old_digits, new_digits) + 1);
        p[0] = 1;
        for (int i = 1; i < (int)p.size(); i++) {
            p[i] = p[i - 1] * 10;
        }
        vector<int> res;
        long long cur = 0;
        int cur_digits = 0;
        for (int i = 0; i < (int)a.size(); i++) {
            cur += p[cur_digits] * a[i];
            cur_digits += old_digits;
            while (cur_digits >= new_digits) {
                res.push_back(cur % p[new_digits]);
                cur /= p[new_digits];
                cur_digits -= new_digits;
            }
        }
        res.push_back(cur);
        while (!res.empty() && !res.back()) {
            res.pop_back();
        }
        return res;
    }

    static vector<long long> karatsubaMultiply(const vector<long long>& a, const vector<long long>& b) {
        int n = (int)a.size();
        vector<long long> res((n << 1), 0);
        if (n <= 32) {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < n; j++) {
                    res[i + j] += a[i] * b[j];
                }
            }
            return res;
        }

        int k = (n >> 1);
        vector<long long> a1(a.begin(), a.begin() + k);
        vector<long long> a2(a.begin() + k, a.end());
        vector<long long> b1(b.begin(), b.begin() + k);
        vector<long long> b2(b.begin() + k, b.end());

        vector<long long> a1b1 = karatsubaMultiply(a1, b1);
        vector<long long> a2b2 = karatsubaMultiply(a2, b2);

        for (int i = 0; i < k; i++) {
            a2[i] += a1[i];
            b2[i] += b1[i];
        }

        vector<long long> r = karatsubaMultiply(a2, b2);
        for (int i = 0; i < (int)a1b1.size(); i++) {
            r[i] -= a1b1[i];
        }
        for (int i = 0; i < (int)a2b2.size(); i++) {
            r[i] -= a2b2[i];
        }

        for (int i = 0; i < (int)r.size(); i++) {
            res[i + k] += r[i];
        }
        for (int i = 0; i < (int)a1b1.size(); i++) {
            res[i] += a1b1[i];
        }
        for (int i = 0; i < (int)a2b2.size(); i++) {
            res[i + n] += a2b2[i];
        }
        return res;
    }

    bignum operator * (const bignum& A) const {
        vector<int> x = convert_base(this->a, base_digits, 6);
        vector<int> y = convert_base(A.a, base_digits, 6);
        vector<long long> a(x.begin(), x.end());
        vector<long long> b(y.begin(), y.end());

        while (a.size() < b.size()) {
            a.push_back(0);
        }
        while (b.size() < a.size()) {
            b.push_back(0);
        }
        while (a.size() & (a.size() - 1)) {
            a.push_back(0);
            b.push_back(0);
        }

        vector<long long> c = karatsubaMultiply(a, b);
        bignum res;
        res.sign = sign * A.sign;
        int carry = 0;
        for (int i = 0; i < (int)c.size(); i++) {
            long long cur = c[i] + carry;
            res.a.push_back(cur % 1000000);
            carry = cur / 1000000;
        }
        res.a = convert_base(res.a, 6, base_digits);
        res.trim();
        return res;
    }

    bignum& operator *= (const bignum& A) { *this = *this * A; return *this; }

    friend pair<bignum, bignum> divmod(const bignum& A, const bignum& B) {
        int norm = base / (B.a.back() + 1);
        bignum a = A.abs() * norm;
        bignum b = B.abs() * norm;
        bignum q, r;
        q.a.resize(a.a.size());

        for (int i = a.a.size() - 1; i >= 0; i--) {
            r *= base;
            r += a.a[i];
            int s1 = (r.a.size() <= b.a.size() ? 0 : r.a[b.a.size()]);
            int s2 = (r.a.size() + 1 <= b.a.size() ? 0 : r.a[b.a.size() - 1]);
            int d = ((long long)base * s1 + s2) / b.a.back();
            r -= b * d;
            while (r < 0) {
                r += b;
                --d;
            }
            q.a[i] = d;
        }

        q.sign = A.sign * B.sign;
        r.sign = A.sign;
        q.trim();
        r.trim();
        return { q, r / norm };
    }

    bignum operator / (const bignum& A) const { return divmod(*this, A).first; }
    bignum& operator /= (const bignum& A) { *this = *this / A; return *this; }
    bignum operator % (const bignum& A) const { return divmod(*this, A).second; }
    bignum& operator %= (const bignum& A) { *this = *this % A; return *this; }

    bignum& operator /= (int A) {
        if (A < 0) {
            sign = -sign;
            A = -A;
        }
        for (int i = (int)a.size() - 1, remain = 0; i >= 0; i--) {
            long long cur = a[i] + (long long)base * remain;
            a[i] = cur / A;
            remain = cur % A;
        }
        trim();
        return *this;
    }

    bignum operator / (int A) { bignum res = *this; res /= A; return res; }

    long long operator % (long long A) {
        if (A < 0) {
            A = -A;
        }
        long long carry = 0;
        for (int i = (int)a.size() - 1; i >= 0; i--) {
            carry = (a[i] + (long long)base * carry) % A;
        }
        return carry * sign;
    }

    int operator % (int A) {
        return (int)(*this % (long long)A);
    }

    long long val() const {
        long long res = 0;
        for (int i = (int)a.size() - 1; i >= 0; i--) {
            res *= base;
            res += a[i];
        }
        return res;
    }
};


struct decryptPair {
    bool status; // true if decrypt successfully (file is not modified by outsider)
    string decryptedString;

    decryptPair(bool _status = 0, string _decryptedString = "") {
        status = _status;
        decryptedString = _decryptedString;
    }
};


class Data_Encryption {
private:
    static const int startingChar = 32;
    static const int baseChar = 95;  // characters in encrypted file range from startingChar -> startingChar + baseChar - 1
    static const int baseHash = 257;
    static const int baseSzBlock = 8; //first, text will be divided into blocks of baseSzBlock and converted into numerical order
    static const int szBlock = (int)1e4; // then, encryption will further be applied to each block of szBlock size
    const string Opening = "-----BEGIN TPT ENCRYPTED DATA-----\n\n";
    const string Ending = "\n\n------END TPT ENCRYPTED DATA------";
    string hashCode; //put in the end of file, used to check integrity of file
    vector<char> Characters; //baseChar letters
    vector<int> T, prime; // T is the encryptionKey-th row in the table t mentioned in encryption method
    deque<char> encryptedCode, decryptedCode;


    // since there are only baseChar different characters in cipher and text
    // sieve only covers prime number from 1 to baseChar
    void sieve() {
        vector<bool> p(baseChar, 0);

        for (int i = 2; i * i < baseChar; i++) {
            if (p[i] == 0) {
                for (int j = i * i; j < baseChar; j += i) {
                    p[j] = 1;
                }
            }
        }

        for (int i = 2; i < baseChar; i++) {
            if (p[i] == 0) {
                prime.push_back(i);
            }
        }
    }


    // returns (A ^ B) % MOD
    int POW(int A, int B, int MOD) {
        int res = 1;
        for (; B > 0; B >>= 1) {
            if (B & 1) {
                res = (1LL * res * A) % MOD;
            }
            A = (1LL * A * A) % MOD;
        }
        return res;
    }


    vector<long long> Hash(string s) {
        int n = (int)sizeof(MOD) / sizeof(MOD[0]);
        vector<long long> res(n);
        int pos = 0;
        for (int i = 0; i < n; i++) {
            long long ans = 0;
            for (char x : s) {
                ans = (ans * baseHash + (int)x) % MOD[i];
            }
            res[pos++] = ans;
        }
        return res;
    }


    // returns the num-th row of the table t mention in encryption method
    // t calculation:
    // t[i][j] is basically the number of ways to go from (0, 0) to (i, j) only going right or down
    // it is observed that the total number of steps is i + j, and total number of times of going right is j
    // so, t[i][j] is number of ways to pick j steps out of i + j steps
    // t[i][j] = C(j, i + j) (or t[i][j] = C(i, i + j), both are same)
    // since there are only baseChar different characters
    // t[i][j] %= baseChar
    vector<int> calc(bignum num) {
        vector<int> cnt((int)prime.size(), 0);
        vector<int> res(szBlock);
        res[0] = 1;
        num = num - 1;
        long long cur = 1;  // cur stores the permanent result (contains only numbers out of prime range, which wont be changed
                            // by future operations)
        int pos = 1;

        // for further optimization
        // notice that t[i][j] = t[i][j - 1] * (i + j) / j
        for (int i = 1; i < szBlock; i++) {
            bignum x = num + i; // *= num + i
            int y = i; // /= i (i and j are interchangable)
            // since we can't directly divide x by y
            // we need to modify the prime's degree influenced by the factor x/y
            for (int j = 0; j < (int)prime.size(); j++) {
                while (x % prime[j] == 0) {
                    x /= prime[j];
                    cnt[j]++;
                }
                while (y % prime[j] == 0) {
                    y /= prime[j];
                    cnt[j]--;
                }
            }
            cur *= (x % baseChar); // x is currently out of prime range, which mean it wont be changed (deleted from answer) by future operations
            cur %= baseChar;
            long long ans = cur;
            for (int j = 0; j < (int)prime.size(); j++) {
                ans *= POW(prime[j], cnt[j], baseChar);
                ans %= baseChar;
            }
            res[pos++] = ans;
        }
        return res;
    }


public:
    // encryption is applied to each block of szBlock characters
    // encryption problem:
    // given a text vector<char> a and the number encryptionKey (in initialize method) (encryptionKey > 0)
    // repeat the process encryptionKey times:
    // b[i] = b[i - 1] + a[i] ; a = b (1)
    // where b is the encrypted vector<char> rendered from a

    // since the complexity of the above code is too large
    // optimization:
    // let t be a 2d array where t[i][j] = t[i - 1][j] + t[i][j - 1], and the ith row of t means
    // (1) has been repeated i + 1 times (since the array starts at 0)
    // base case: t[0][i] = t[i][0] = 1
    // optimized t calculation: see calc()
    // b is calculated:
    // b[pos] = sum of (a[pos - j] * t[encryptionKey - 1][j]) for j from 0 to pos
    string Encrypt(string textString) {
        for (char hashcode : hashCode) {
            textString.push_back(hashcode);
        }

        //firstly, divide text into blocks of baseSzBlock, longest numerical order's length is baseSzBlock * 2
        deque<char> text;
        for (int i = 0; i < textString.size(); i += baseSzBlock) {
            //compute numerical order of substring textString(i, i + baseSzBlock - 1)
            bignum num = 0;
            int posEndBlock = min(i + baseSzBlock, (int)textString.size());
            for (int j = i; j < posEndBlock; j++) {
                num *= baseChar + 1;  //since there is an added blank character (in case there isnt enough baseSzBlock letters), base will be baseChar + 1
                num += (int)textString[j] - startingChar + 1;
            }


            for (char c : num.to_string(baseSzBlock << 1)) {
                text.push_back(c);
            }
        }

        vector<int> encryptedVector(text.size()); // the result, encryptedCode, will be: encryptedCode[i] = Character[encryptedVector[i]]
        //since characters in text range from startingChar and above, convert them to base 0 for easier calculation
        for (int i = 0; i < text.size(); i++) {
            encryptedVector[i] = (int)text[i] - startingChar;
        }

        for (int i = (int)text.size() - 1; i >= 0; i--) {
            int res = 0;
            int pos = i % szBlock;
            for (int j = 0; j <= pos; j++) {
                res = (res + T[j] * encryptedVector[i - j]) % baseChar;
            }
            encryptedVector[i] = res;
        }

        encryptedCode.resize(encryptedVector.size());
        for (int i = 0; i < encryptedVector.size(); i++) {
            encryptedCode[i] = Characters[encryptedVector[i]];
        }

        // prettify encrypted code
        // for fun
        for (auto i = Opening.rbegin(); i != Opening.rend(); i++) {
            encryptedCode.push_front(*i);
        }
        for (auto i = Ending.begin(); i != Ending.end(); i++) {
            encryptedCode.push_back(*i);
        }


        string encryptedString = "";
        for (char x : encryptedCode) {
            encryptedString += x;
        }

        return encryptedString;
    }


    decryptPair Decrypt(string cipherString) {
        decryptPair failedDecryption = decryptPair(0);
        deque<char> cipher;

        for (char x : cipherString) {
            cipher.push_back(x);
        }

        if (cipher.size() < Opening.size() + Ending.size()) {
            return failedDecryption;
        }

        //rendering cipher
        for (auto i = Opening.begin(); i != Opening.end(); i++) {
            if (cipher.front() != *i) {
                if ((int)*i == 10 || (int)*i == 13) {
                    if ((int)cipher.front() == 10 || (int)cipher.front() == 13) {
                        continue;
                    }
                }
                return failedDecryption;
            }
            cipher.pop_front();
        }
        for (auto i = Ending.rbegin(); i != Ending.rend(); i++) {
            if (cipher.back() != *i) {
                if ((int)*i == 10 || (int)*i == 13) {
                    if ((int)cipher.back() == 10 || (int)cipher.back() == 13) {
                        continue;
                    }
                }
                return failedDecryption;
            }
            cipher.pop_back();
        }


        //first letter of cipher is always the same as first letter of text (decryptedVector)
        //cipher[i] = 1 * text[i] + T2 * text[i - 1] + T3 * text[i - 2] + ...
        //hence, to retrieve text[i], we only need to calculate cipher[i] - (T2 * text[i - 1] + T3 * text[i - 2] + ...)
        //text[i - 1], text[i - 2], ... are all known before reaching to i
        vector<int> decryptedVector(cipher.size()); //decryptedCode[i] = Character[decryptedVector[i]]
        decryptedCode.resize(decryptedVector.size());

        for (int i = 0; i < cipher.size(); i++) {
            decryptedVector[i] = (int)cipher[i] - startingChar;
        }
        for (int i = 0; i < cipher.size(); i++) {
            int res = decryptedVector[i];
            int pos = i % szBlock;
            for (int j = 1; j <= pos; j++) {
                res = (res - (T[j] * decryptedVector[i - j]) % baseChar + baseChar) % baseChar;
            }
            decryptedVector[i] = (int)Characters[res] - startingChar;
            decryptedCode[i] = Characters[decryptedVector[i]];
        }

        //process blocks of baseSzBlock to convert to original text
        if (decryptedCode.size() % (baseSzBlock << 1) != 0) {
            return failedDecryption;
        }
        deque<char> text;
        for (int i = 0; i < decryptedCode.size(); i += (baseSzBlock << 1)) {
            bignum num = 0;
            for (int j = i; j < i + (baseSzBlock << 1); j++) {
                num *= 10;
                num += decryptedCode[j] - '0';
            }

            bignum Coef = 1; //used to render original text, letters will be recovered from left to right (high to low)
            for (int j = 1; j < baseSzBlock; j++) {
                Coef *= baseChar + 1;
            }
            while (num > 0) {
                for (int j = 0; j <= baseChar + 1; j++) {
                    if (num < Coef) {
                        if (j != 0) {
                            text.push_back(Characters[j - 1]);
                        }
                        break;
                    }
                    else {
                        if (j == baseChar + 1) {
                            return failedDecryption;
                        }
                        num -= Coef;
                    }
                }
                Coef /= baseChar + 1;
            }
        }

        // render hash to check integrity
        int sz = sizeof(MOD) / sizeof(MOD[0]);
        sz++;
        string tmpHash = "";

        while (!text.empty()) {
            tmpHash += text.back();
            if (text.back() == '-') {
                sz--;
            }
            text.pop_back();
            if (sz == 0) {
                break;
            }
        }
        reverse(tmpHash.begin(), tmpHash.end());

        if (tmpHash != hashCode) {
            return failedDecryption;
        }

        string decryptedString = "";
        for (char x : text) {
            decryptedString += x;
        }

        return decryptPair(1, decryptedString);
    }

    Data_Encryption() {
        sieve();

        for (int i = startingChar; i < startingChar + baseChar; i++) {
            Characters.push_back(char(i));
        }
    }


    // import secret key
    // secret key is used to encrypt and decrypt file
    void Initialize(string secretKey) {
        bignum encryptionKey = 0;
        bignum cur = 1;

        for (char x : secretKey) {
            encryptionKey += cur * (int)x;
            cur *= baseChar;
        }

        T = calc(encryptionKey);

        vector<long long> hashVector = Hash(encryptionKey.to_string());
        hashCode = "-";
        for (long long x : hashVector) {
            hashCode += to_string(x);
            hashCode += '-';
        }
    }
};

Data_Encryption dataEncryption = Data_Encryption();


string Decrypt(string cipher) {
    decryptPair decrypt = dataEncryption.Decrypt(cipher);

    if (decrypt.status == 0) {
		return "__invalid_cipher__";
	}

    return decrypt.decryptedString;
}


string Encrypt(string text) {
    return dataEncryption.Encrypt(text);
}


void Initialize(string secretKey) {
    dataEncryption.Initialize(secretKey);
}


int main(){
	Initialize("TroyCode");

	string text = "text data to be processed";

	string encryptedText = Encrypt(text);
	cout << "Encrypted text:\n";
	cout << encryptedText;

	cout << "\n\n\n";

	// check if decrypted cipher is the same as the original text
	string decryptedCipher = Decrypt(encryptedText);
	cout << "Decrypted cipher:\n";
	cout << decryptedCipher;

	cout << "\n";
}

